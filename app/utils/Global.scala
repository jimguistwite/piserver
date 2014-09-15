package utils

import play.api.Application
import play.api.GlobalSettings
import service.TemperatureReader

object Global extends GlobalSettings {

  var myApp: Application = _
  var gpio: service.Gpio = _
  var temp: service.TemperatureReader = _

  override def onStart(app: Application) {
    myApp = app
    gpio = new service.Gpio()
    temp = new TemperatureReader()
  }

  override def onStop(app: Application) {
    gpio.shutdown()
    temp.shutdown()
  }
}
