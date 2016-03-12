package service

import com.pi4j.io.gpio._
import com.pi4j.io.gpio.event.{GpioPinDigitalStateChangeEvent, GpioPinListenerDigital}
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import org.slf4j.LoggerFactory
import play.api._
import play.api.libs.json.Json
import utils.Global

/**
 * Beginnings of a Gpio controller
 */
class Gpio extends GpioPinListenerDigital {

  val logger = LoggerFactory.getLogger(classOf[Gpio])
  val gpio = GpioFactory.getInstance()
  var pinMap = Map[Int, GpioPinDigital]()
  var pinLabelMap = Map[String, GpioPinDigital]()

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
            logger.debug(s"look for $nm to be a digital output pin")
            val fld = cls.getField(nm)
            val v = fld.get(null).asInstanceOf[Pin]
            val p = gpio.provisionDigitalOutputPin(v, label, PinState.LOW)
            pinMap += i -> p
            pinLabelMap += label -> p
          case "digitalinput" =>
            val cls = classOf[RaspiPin]
            val nm = f"GPIO_$i%02d"
            logger.debug(s"look for $nm to be a digital input pin")
            val fld = cls.getField(nm)
            val v = fld.get(null).asInstanceOf[Pin]
            val p = gpio.provisionDigitalInputPin(v, label, PinPullResistance.PULL_DOWN)
            // create and register gpio pin listener
            p.addListener(this)
            pinMap += i -> p
            pinLabelMap += label -> p
          case _ =>
            logger.error(s"invalid mode $mode")
        }
      case _ =>
    }
  })

  @Override
  def handleGpioPinDigitalStateChangeEvent(event: GpioPinDigitalStateChangeEvent) {
    logger.debug("GPIO PIN STATE CHANGE: " + event.getPin + " = " + event.getState)
    val now = DateTime.now()
    val ts = ISODateTimeFormat.dateTimeNoMillis().print(now)
    val pin = pinLabelMap.find(item=>{
      item._2.getName.equals(event.getPin.getName)
    }) match {
      case Some(pinLabelMatch) => pinLabelMatch._1
      case None => event.getPin.getName
    }
    val state = event.getState.getName
    val j = Json.obj("status"->"success", "event"->Map("eventtype" -> "gpio", "ts" -> ts, "pin" -> pin, "state" -> state))
    Global.eventManager.processEvent(j.toString())
  }

  def shutdown() {
    gpio.shutdown()
  }

  def setPin(pin: String, pinStateHigh: Boolean) {
    logger.debug(s"set pin $pin to $pinStateHigh")
    pinLabelMap.get(pin) match {
      case Some (pin:GpioPinDigitalOutput) =>
        gpio.setState(pinStateHigh, pin)
      case None =>
        logger.error(s"unknown pin $pin")
      case _ =>
        logger.error(s"pin is incorrect type - possibly an input pin?")
    }
  }
}
