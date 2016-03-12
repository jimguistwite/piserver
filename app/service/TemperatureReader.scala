package service

import java.io.File

import com.typesafe.config.ConfigRenderOptions
import org.slf4j.LoggerFactory
import play.api.Play

import scala.io.Source
import scala.util.{Failure, Success, Try}
import play.api.Play.current

/**
 * Provide access to temperature sensors.
 */
class TemperatureReader {

  val logger = LoggerFactory.getLogger(classOf[TemperatureReader])

  Runtime.getRuntime.exec("modprobe w1-gpio")
  Runtime.getRuntime.exec("modprobe w1-therm")
  val dir = new File("/sys/bus/w1/devices")
  logger.debug(s"devices ${dir.list()}")

  def getSensorName(f:File): String = {
    logger.debug("get sensor for file " + f.getName)
    val cfg = Play.current.configuration
    cfg.getConfig("temp") match {
      case Some(tmp) =>
        tmp.entrySet.find(item=>{
          val v = item._2.render(ConfigRenderOptions.concise())
          val v1 = v.replace("\"","")
          logger.debug(s"compare $v1 to ${f.getName}")
          v1 == f.getName
        }) match {
          case Some(item) => item._1
          case None => "NA"
        }
      case None =>
        "NA"
    }
  }

  def getAll(): List[(String,Double,Double)] = {
    dir.listFiles().flatMap(f=> {
      val meas = new File(f, "w1_slave")
      if (meas.exists) {
        convertMeasurementFile(meas) match {
          case Success(v) => List((getSensorName(f),v._1, v._2))
          case _ => List()
        }
      }
      else {
        List()
      }
    }).toList
  }

  /*
  sudo modprobe w1-gpio
sudo modprobe w1-therm
cd /sys/bus/w1/devices
ls
cd 28-xxxx (change this to match what serial number pops up)
cat w1_slave
   */

  def getTemp(deviceId: String): Try[(Double,Double)] = {
    logger.debug(s"read temp from device $deviceId")

    val cfg = Play.current.configuration
    val internalid = cfg.getString("temp." + deviceId).getOrElse(deviceId)
    logger.debug(s"read temp from internal device $internalid")

    val f = new File(dir, internalid + "/" + "w1_slave")
    if (f.exists) {
      convertMeasurementFile(f)
    }
    else {
      Failure(new RuntimeException(s"${f.getCanonicalPath} not found"))
    }
  }

  def convertMeasurementFile(f: File): Try[(Double,Double)] = {
    logger.debug(s"read file ${f.getName}")
    val i = Source.fromFile(f).getLines()
    val first = i.next()
    val second = i.next()
    if (first.endsWith("YES")) {
      second.lastIndexOf("t=") match {
        case -1 =>
          logger.error(s"invalid data in second line $second")
          Failure(new RuntimeException(s"unable to read ${f.getName}- invalid response"))
        case idx: Int =>
          val tmp = second.substring(idx + 2).toDouble / 1000.0
          Success(tmp, tmp * 9.0 / 5.0 + 32.0)
      }
    }
    else {
      Failure(new RuntimeException(s"unable to read ${f.getName} - temp error - NO"))
    }
  }

  def shutdown() {

  }


}
