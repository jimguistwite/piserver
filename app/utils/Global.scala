package utils

import play.api.Application
import play.api.GlobalSettings
import service.{ITachManager, EventManager, CM11A, TemperatureReader}

object Global extends GlobalSettings {

  var myApp: Application = _
  var gpio: service.Gpio = _
  var temp: service.TemperatureReader = _
  var cm11a: service.CM11A = _
  var eventManager: service.EventManager = _
  var itachManager: service.ITachManager = _

  override def onStart(app: Application) {
    myApp = app
    //gpio = new service.Gpio()
    //temp = new TemperatureReader()
    //cm11a = new CM11A
    //cm11a.init()
    //eventManager = new EventManager
    //eventManager.init()
    itachManager = new ITachManager
    itachManager.init()
  }

  override def onStop(app: Application) {
    //gpio.shutdown()
    //temp.shutdown()
    //cm11a.shutdown()
    //eventManager.shutdown()
    itachManager.shutdown()
  }
}
