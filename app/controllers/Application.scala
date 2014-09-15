package controllers

import org.slf4j.LoggerFactory
import play.api._
import play.api.libs.json._
import play.api.mvc._
import play.libs.{Akka}
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
    Ok(JsObject("hello"->JsString("World") :: Nil))
  }

  def systemInfo = Action {
    Ok(JsObject("Serial Number"->JsString(SystemInfo.getSerial) ::
      "CPU Revision"->JsString(SystemInfo.getCpuRevision) ::
    "CPU Architecture"->JsString(SystemInfo.getCpuArchitecture) ::
    "CPU Part"->JsString(SystemInfo.getCpuPart) ::
    "CPU Temperature" -> JsNumber(SystemInfo.getCpuTemperature.toDouble) ::
    "CPU Core Voltage" -> JsNumber(SystemInfo.getCpuVoltage.toDouble) ::
    "MIPS" -> JsString(SystemInfo.getBogoMIPS) ::
    "Hardware Revision" -> JsString(SystemInfo.getRevision) ::
    "Is Hard Float ABI" -> JsBoolean(SystemInfo.isHardFloatAbi) ::
    "Board Type" -> JsString(SystemInfo.getBoardType.name())
    :: Nil))
  }

  def memoryInfo = Action {
    Ok(JsObject("Total Memory"->JsNumber(SystemInfo.getMemoryTotal) ::
      "Used Memory"->JsNumber(SystemInfo.getMemoryFree) ::
      "Shared Memory"->JsNumber(SystemInfo.getMemoryShared) ::
      "Memory Buffers"->JsNumber(SystemInfo.getMemoryBuffers) ::
      "Cached Memory"->JsNumber(SystemInfo.getMemoryCached) ::
      "SDRAM_C Voltage"->JsNumber(SystemInfo.getMemoryVoltageSDRam_C.toDouble) ::
      "SDRAM_I Voltage"->JsNumber(SystemInfo.getMemoryVoltageSDRam_I.toDouble) :: Nil))
  }

  def osInfo = Action {
    Ok(JsObject("OS Name"->JsString(SystemInfo.getOsName) ::
      "OS Version"->JsString(SystemInfo.getOsVersion) ::
      "OS Architecture"->JsString(SystemInfo.getOsArch) ::
      "OS Firmware Build"->JsString(SystemInfo.getOsFirmwareBuild) ::
      "OS Firmware Date"->JsString(SystemInfo.getOsFirmwareDate) :: Nil))
  }

  def javaInfo = Action {
    Ok(JsObject("Java Vendor"->JsString(SystemInfo.getJavaVendor) ::
      "Java Version"->JsString(SystemInfo.getJavaVersion) ::
      "Java VM"->JsString(SystemInfo.getJavaVirtualMachine) ::
      "Java Runtime"->JsString(SystemInfo.getJavaRuntime) :: Nil))
  }

  def networkInfo = Action {
    var rv = JsObject("Hostname"->JsString(NetworkInfo.getHostname) :: Nil)
    rv = rv ++ JsObject("ips"->JsArray(NetworkInfo.getIPAddresses.map(ip=>JsString(ip))) :: Nil)
    rv = rv ++ JsObject("fqdn"->JsArray(NetworkInfo.getFQDNs.map(ip=>JsString(ip))) :: Nil)
    rv = rv ++ JsObject("nameservers"->JsArray(NetworkInfo.getNameservers.map(ip=>JsString(ip))) :: Nil)

    Ok(rv)
  }

   def gpioState = Action {
    import scala.collection.JavaConversions._
    var pins = JsArray()
    utils.Global.gpio.gpio.getProvisionedPins.foreach(pin => {
      pins = pins :+ JsObject(pin.getName->JsNumber(pin.getPin.getAddress)::Nil)
    })

    Ok(JsObject("pins"->pins :: Nil))
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

    results.map {opresultSeq =>
      opresultSeq.forall(r => {
      val res = (r \ "status").as[String]
      "success" == res
    }) match {
      case true => Ok(JsObject("status" -> JsString("success") :: "results" -> JsArray(opresultSeq) :: Nil))
      case false => Ok(JsObject("status" -> JsString("failed") :: "results" -> JsArray(opresultSeq) :: Nil))
    }
  }
  }

  def getTemp(req: JsObject): JsObject = {
    (req \ "id").asOpt[String] match {
      case Some(id) =>
        utils.Global.temp.getTemp (id) match {
          case Success (temp) => JsObject ("status" -> JsString ("success") :: "c" -> JsNumber (temp._1) :: "f" -> JsNumber(temp._2) :: Nil)
          case Failure (e) => JsObject ("status" -> JsString ("failed") :: "error" -> JsString (e.getMessage) :: Nil)
        }
      case _ =>
        JsObject ("status" -> JsString ("failed") :: "error" -> JsString ("missing id param") :: Nil)
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
        JsObject("status"->JsString("success") :: Nil)
      case "wait" =>
        val tm = (op \ "duration").as[Long]
        Thread.sleep(tm)
        JsObject("status"->JsString("success") :: Nil)
      case "gettemp" =>
        val id = (op \ "id").as[String]
        utils.Global.temp.getTemp(id) match {
          case Success(temp) => JsObject("status"->JsString("success") :: "c"->JsNumber(temp._1) :: "f"->JsNumber(temp._2) :: Nil)
          case Failure(e) => JsObject("status"->JsString("failed") :: "error"->JsString(e.getMessage) :: Nil)
        }
      case _ =>
        JsObject("status"->JsString("failed") :: "error"->JsString("unknown operation") :: Nil)
    }
  }
}