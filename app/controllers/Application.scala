package controllers

import java.net.URL

import com.pi4j.io.gpio.{GpioPinDigitalOutput, GpioPinDigitalInput, PinMode}
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import org.slf4j.LoggerFactory
import play.api._
import play.api.libs.json._
import play.api.mvc._
import play.libs.Akka
import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.concurrent.Execution.Implicits._

import com.pi4j.system.{NetworkInfo, SystemInfo}

import scala.util.{Failure, Success}

object Contexts {
  val myExecutionContext: ExecutionContext = Akka.system.dispatchers.lookup("gpio-context")
}
object Application extends Controller {
  val logger = LoggerFactory.getLogger(classOf[Application])

  def index = Action {
    Ok(JsObject("hello" -> JsString("World") :: Nil))
  }

  def systemInfo = Action {
    Ok(Json.obj(
      "status" -> "success",
      "sysinfo" -> Map(
        "Serial Number" -> JsString(SystemInfo.getSerial),
        "CPU Temperature" -> JsNumber(SystemInfo.getCpuTemperature.toDouble),
        "CPU Revision" -> JsString(SystemInfo.getCpuRevision),
        "CPU Architecture" -> JsString(SystemInfo.getCpuArchitecture),
        "CPU Part" -> JsString(SystemInfo.getCpuPart),
        "CPU Core Voltage" -> JsNumber(SystemInfo.getCpuVoltage.toDouble),
        "Hardware Revision" -> JsString(SystemInfo.getRevision),
        "Is Hard Float ABI" -> JsBoolean(SystemInfo.isHardFloatAbi),
        "Board Type" -> JsString(SystemInfo.getBoardType.name()))))
  }

  def memoryInfo = Action {
    Ok(Json.obj(
      "status" -> "success",
      "memoryinfo" -> Map(
        "Total Memory" -> JsNumber(SystemInfo.getMemoryTotal),
        "Used Memory" -> JsNumber(SystemInfo.getMemoryUsed),
        "Free Memory" -> JsNumber(SystemInfo.getMemoryFree),
        "Shared Memory" -> JsNumber(SystemInfo.getMemoryShared),
        "Memory Buffers" -> JsNumber(SystemInfo.getMemoryBuffers),
        "Cached Memory" -> JsNumber(SystemInfo.getMemoryCached),
        "SDRAM_C Voltage" -> JsNumber(SystemInfo.getMemoryVoltageSDRam_C.toDouble),
        "SDRAM_I Voltage" -> JsNumber(SystemInfo.getMemoryVoltageSDRam_I.toDouble))))
  }

  def osInfo = Action {
    Ok(Json.obj(
      "status" -> "success",
      "osinfo" -> Map(
        "OS Name" -> JsString(SystemInfo.getOsName),
        "OS Version" -> JsString(SystemInfo.getOsVersion),
        "OS Architecture" -> JsString(SystemInfo.getOsArch),
        "OS Firmware Build" -> JsString(SystemInfo.getOsFirmwareBuild),
        "OS Firmware Date" -> JsString(SystemInfo.getOsFirmwareDate))))
  }

  def javaInfo = Action {
    Ok(Json.obj(
      "status" -> "success",
      "javainfo" -> Map(
        "Java Vendor" -> JsString(SystemInfo.getJavaVendor),
        "Java Version" -> JsString(SystemInfo.getJavaVersion),
        "Java VM" -> JsString(SystemInfo.getJavaVirtualMachine),
        "Java Runtime" -> JsString(SystemInfo.getJavaRuntime))))
  }

  def networkInfo = Action {
    Ok(Json.obj(
      "status" -> "success",
      "networkinfo" -> Map(
        "Hostname" -> JsString(NetworkInfo.getHostname),
        "ips" -> JsArray(NetworkInfo.getIPAddresses.map(ip => JsString(ip))),
        "fqdn" -> JsArray(NetworkInfo.getFQDNs.map(ip => JsString(ip))),
        "nameservers" -> JsArray(NetworkInfo.getNameservers.map(ip => JsString(ip))))))
  }

  def gpioState = Action {
    import scala.collection.JavaConversions._
    Ok(Json.obj(
      "status" -> "success",
      "gpiostate" -> JsArray(utils.Global.gpio.gpio.getProvisionedPins.map(pin => {
        val o = Json.obj("name" -> pin.getName, "mode" -> pin.getMode.toString, "address" -> pin.getPin.getAddress)
        if (pin.getMode.getValue == PinMode.DIGITAL_INPUT.getValue) {
          o +("state", JsString(pin.asInstanceOf[GpioPinDigitalInput].getState.getName))
        }
        else {
          o +("state", JsString(pin.asInstanceOf[GpioPinDigitalOutput].getState.getName))
        }
      }).toList)))
  }

  def gpioStateForItem(id: String) = Action {
    import scala.collection.JavaConversions._
    utils.Global.gpio.gpio.getProvisionedPins.find(pin => pin.getName == id) match {
      case Some(pin) =>
        var o = Json.obj("name" -> pin.getName, "mode" -> pin.getMode.toString, "address" -> pin.getPin.getAddress)
        if (pin.getMode.getValue == PinMode.DIGITAL_INPUT.getValue) {
          o = o +("state", JsString(pin.asInstanceOf[GpioPinDigitalInput].getState.getName))
        }
        else {
          o = o +("state", JsString(pin.asInstanceOf[GpioPinDigitalOutput].getState.getName))
        }
        Ok(Json.obj("status" -> "success", "gpiostate" -> o))

      case None => Ok(Json.obj("status" -> "failed", "message" -> s"unknown pin ${id}"))
    }
  }

  def x10State = Action {
    import scala.collection.JavaConversions._
    val cfg = Play.current.configuration
    val knownCodes = cfg.getStringList("knownX10codes").getOrElse(new java.util.ArrayList())

    val itemsThatAreOn = utils.Global.cm11a.getState
    Ok(Json.obj(
      "status" -> "success",
      "x10state" -> JsArray(
        knownCodes.map(code => {
          val b = itemsThatAreOn.contains(code)
          Json.obj("code" -> code, "status" -> {
            if (b) "on" else "off"
          })
        }))))
  }

  /**
   * Get the on/off state for an item.
   * @param houseunitcode
   * @return
   */
  def x10StateForItem(houseunitcode: String) = Action {
    logger.debug(s"get x10 state for item ${houseunitcode}")
    val lst = utils.Global.cm11a.getState.filter(item => {
      item == houseunitcode
    })
    val now = new DateTime()
    val dt = ISODateTimeFormat.dateTime().print(now)
    if (lst.isEmpty) Ok(Json.obj("status" -> "success", "x10state" -> Json.obj("code" -> houseunitcode, "status" -> "off")))
    else Ok(Json.obj("status" -> "success", "x10state" -> Json.obj("code" -> houseunitcode, "status" -> "on")))
  }

  /**
   * Perform an X10 operation.
   * @return
   */
  def x10 = Action.async(parse.json) { request =>
    val houseCodeUnit = (request.body \ "housecodeunit").as[String]
    val function = (request.body \ "function").as[String]
    val option = (request.body \ "option").as[Option[String]]
    utils.Global.cm11a.x10(houseCodeUnit, function, option) map (s => {
      if (s.length == 0) {
        Ok(Json.obj("status" -> "success"))
      }
      else {
        Ok(Json.obj("status" -> "failed", "message" -> s))
      }
    })
  }

  def temp(deviceId: String) = Action { request =>
    Ok(getTemp(Json.obj("id" -> deviceId)))
  }

  def register = Action(parse.json) { request =>
    val client = (request.body \ "client").as[String]
    val url = (request.body \ "url").as[String]
    utils.Global.eventManager.register(client, new URL(url))
    Ok(Json.obj("status" -> "success"))
  }

  def performOps = Action.async(parse.json) { request =>
    val ops = (request.body \ "ops").as[JsArray]
    val results = (request.body \ "synchronized").asOpt[Boolean].getOrElse(false) match {
      case false =>
        Future {
          logger.debug("using built in execution context")
          ops.value.map(item => performOp(item.as[JsObject]))
        }
      case true =>
        // need to perform each of the operations such that no
        // other performOps request can be running at the same time.
        Future {
          logger.debug("using synchronized execution context")
          val rv = ops.value.map(item => performOp(item.as[JsObject]))
          logger.debug("done using synchronized execution context")
          rv
        }(Contexts.myExecutionContext)
    }

    results.map { opresultSeq =>
      opresultSeq.forall(r => {
        val res = (r \ "status").as[String]
        "success" == res
      }) match {
        case true => Ok(JsObject("status" -> JsString("success") :: "results" -> JsArray(opresultSeq) :: Nil))
        case false => Ok(JsObject("status" -> JsString("failed") :: "results" -> JsArray(opresultSeq) :: Nil))
      }
    }
  }

  def getalltemp() = Action { request =>
    def all = utils.Global.temp.getAll()
    Ok(Json.obj(
      "status" -> "success",
      "temperature" -> JsArray(all.map(item => {
        Json.obj("sensor" -> item._1, "c" -> item._2, "f" -> item._3)
      })
      )))
  }

  def gpioToggle = Action(parse.json) { request =>
    val pin = (request.body \ "pin").as[String]
    utils.Global.gpio.setPin(pin, true)
    Thread.sleep(500)
    utils.Global.gpio.setPin(pin, false)
    Ok(Json.obj("status" -> "success"))
  }

  def getTemp(req: JsObject): JsObject = {
    (req \ "id").asOpt[String] match {
      case Some(id) =>
        utils.Global.temp.getTemp(id) match {
          case Success(temp) => Json.obj(
            "status" -> "success",
            "temperature" -> Json.obj("sensor" -> id, "c" -> temp._1, "f" -> temp._2))
          case Failure(e) => Json.obj("status" -> "failed", "message" -> e.getMessage)
        }
      case _ =>
        Json.obj("status" -> "failed", "message" -> "missing id param")
    }
  }

  def performOp(op: JsObject): JsObject = {
    (op \ "op").as[String] match {
      case "setpin" =>
        val pin = (op \ "pin").as[String]
        (op \ "high").asOpt[Boolean] match {
          case Some(true) => utils.Global.gpio.setPin(pin, true)
          case Some(false) => utils.Global.gpio.setPin(pin, false)
          case _ => {}
        }
        (op \ "low").asOpt[Boolean] match {
          case Some(true) => utils.Global.gpio.setPin(pin, false)
          case Some(false) => utils.Global.gpio.setPin(pin, true)
          case _ => {}
        }
        JsObject("status" -> JsString("success") :: Nil)
      case "wait" =>
        val tm = (op \ "duration").as[Long]
        Thread.sleep(tm)
        JsObject("status" -> JsString("success") :: Nil)
      case _ =>
        JsObject("status" -> JsString("failed") :: "error" -> JsString("unknown operation") :: Nil)
    }
  }

  def getITachNet = Action.async { request =>
    utils.Global.itachManager.getNetworkConfig.map(s=>Ok(s))
  }

  def sendIr = Action.async(parse.json) { request =>
    val command = (request.body \ "command").as[String]
    val where = (request.body \ "where").as[String]
    utils.Global.itachManager.sendIr(where, command).map(s=>Ok(s))
  }
}