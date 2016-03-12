package service

import java.net.URL

import org.slf4j.LoggerFactory
import play.api.libs.ws.WS
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.Play

/**
 * Simple handler of events.
 */

class EventManager {

  val logger = LoggerFactory.getLogger(classOf[EventManager])

  var listeners = Map[String,URL]()

  def register(consumer: String, url: URL): Unit = {
    listeners = listeners.updated(consumer,url)
  }

  def processEvent(event: String): Unit = {
    val cfg = Play.current.configuration
    logger.debug("process event {}", event)
    listeners.foreach(item=>{
      logger.debug(s"invoke ${item._1} with event $event} at ${item._2.toExternalForm}")
      WS.url(item._2.toExternalForm).withHeaders(("Accept", "application/json")).post(event).map { response =>
        val body = response.body
        logger.debug(s"response$body")
      }
    })
  }

  def init(): Unit = {
    val cfg = Play.current.configuration
    cfg.getString("smartthingshub") match {
      case (Some(ssh)) =>
        logger.info("registering smart things hub URL as " + ssh)
        listeners = listeners.updated("hub", new URL(ssh))
      case None =>
        logger.info("no hub predefined in config")
    }
  }

  def shutdown(): Unit = {

  }
}
