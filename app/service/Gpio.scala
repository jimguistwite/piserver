package service

import com.pi4j.io.gpio._
import org.slf4j.LoggerFactory
import play.api._

/**
 * Beginnings of a Gpio controller
 */
class Gpio {

  val logger = LoggerFactory.getLogger(classOf[Gpio])
  val gpio = GpioFactory.getInstance()
  var pinMap = Map[Int, GpioPinDigitalOutput]()
  var pinLabelMap = Map[String, GpioPinDigitalOutput]()

  val cfg = Play.current.configuration
  List.range(0,20).foreach(i=>{
    cfg.getConfig(s"pin.$i") match {
      case Some(c) =>
        val mode = c.getString("mode").getOrElse("digitaloutput")
        val label = c.getString("label").getOrElse("Unknown")
        mode match {
          case "digitaloutput" =>
            val cls = classOf[RaspiPin]
            val nm = f"GPIO_$i%02d"
            logger.debug(s"look for $nm")
            val fld = cls.getField(nm)
            val v = fld.get(null).asInstanceOf[Pin]
            val p = gpio.provisionDigitalOutputPin(v, label, PinState.LOW)
            pinMap += i -> p
            pinLabelMap += label -> p
          case _ =>
            logger.error(s"invalid mode $mode")
        }
      case _ => {

      }
    }

  })

  def shutdown() {
    gpio.shutdown()
  }

  def setPin(pin: String, pinStateHigh: Boolean) {
    logger.debug(s"set pin $pin to $pinStateHigh")
    pinLabelMap.get(pin) match {
      case Some (pin) => gpio.setState(pinStateHigh, pin)
      case None =>
        logger.error(s"unknown pin $pin")
    }
  }
}
