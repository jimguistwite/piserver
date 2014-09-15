package service

import java.io.File

import org.slf4j.LoggerFactory

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * Provide access to temperature sensors.
 */
class TemperatureReader {

  val logger = LoggerFactory.getLogger(classOf[Gpio])

  Runtime.getRuntime.exec("modprobe w1-gpio")
  Runtime.getRuntime.exec("modprobe w1-therm")
  val dir = new File("/sys/bus/w1/devices")
  logger.debug(s"devices ${dir.list()}")

  /*
  sudo modprobe w1-gpio
sudo modprobe w1-therm
cd /sys/bus/w1/devices
ls
cd 28-xxxx (change this to match what serial number pops up)
cat w1_slave
   */

  def getTemp(deviceId: String): Try[(Double,Double)] = {
    logger.debug(s"read temp from device ${deviceId}")
    val f = new File(dir, deviceId + "/" + "w1_slave")
    if (f.exists) {
      logger.debug(s"read file ${f.getName}")
      val i = Source.fromFile(f).getLines()
      val first = i.next()
      val second = i.next()
      if (first.endsWith("YES")) {
        second.lastIndexOf("t=") match {
          case -1 =>
            logger.error(s"invalid data in second line ${second}")
            Failure(new RuntimeException(s"unable to read $deviceId - invalid response"))
          case idx: Int =>
            val tmp = second.substring(idx + 2).toDouble / 1000.0
            Success(tmp, tmp*9.0/5.0 + 32.0)
        }
      }
      else {
        Failure(new RuntimeException(s"unable to read $deviceId - temp error - NO"))
      }
    }
    else {
      Failure(new RuntimeException(s"${f.getCanonicalPath} not found"))
    }
  }
  def shutdown() {

  }


}
