package service

import java.util.concurrent.{TimeUnit, LinkedBlockingQueue}

import org.joda.time.format.{ISODateTimeFormat, DateTimeFormat}
import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import play.api.Play
import play.api.libs.json._
import utils.Global
import scala.sys.process._

import scala.concurrent.{Promise, Future}

/**
 * CM11A X10 interface controller
 */
class CM11A extends Runnable {
  val logger = LoggerFactory.getLogger(classOf[CM11A])
  //var serial: Serial = _
  var queue = new LinkedBlockingQueue[Request]()
  var outstandingMessage: Option[Request] = None
  var thread: Thread = _
  var listenthread: Thread = _
  var heyu: String = _

  case class Request(houseCode: Char, unitCode: Char, function: String, p: Promise[String], arg: Option[String] = None) {

  }

  def init(): Unit = {
    val cfg = Play.current.configuration
    heyu = cfg.getString("heyu").getOrElse("/usr/local/bin/heyu")

    try {
      val cmd = s"$heyu start"
      logger.debug(s"exec $cmd")
      val output = cmd.!!
      logger.debug(s"start result $output")
    }
    catch {
      case (e: Exception) => logger.error("Exception caught starting heyu")
    }

    thread = new Thread(this)
    thread.start()

    listenthread = new Thread(new Runnable() {
      var pendingCodeAndUnit: Option[String] = None

      def run(): Unit = {
        try {
          val cmd = s"$heyu monitor"
          logger.debug(s"exec $cmd")
          cmd.lineStream.foreach(line => {
            try {
              logger.debug("Monitor got " + line)
              //01/31 17:24:44  rcvi addr unit       1 : hu A1  (_no_alias_)
              //Array[String] = Array(01/31, 17:24:44, rcvi, addr, unit, 1, :, hu, A1, (_no_alias_))
              //1/31 17:26:29  rcvi func          Off : hc A
              val items = line.split(' ').filter(_.trim().length != 0)
              if ((items.length > 2) && ("rcvi" == items(2))) {
                if ("addr" == items(3)) {
                  pendingCodeAndUnit = Some(items(8))
                }
                else if ("func" == items(3)) {
                  if (pendingCodeAndUnit.isDefined) {
                    val now = DateTime.now()
                    val dt = DateTime.parse(items(0) + " " + items(1), DateTimeFormat.forPattern("MM/dd HH:mm:ss")).withYear(now.getYear)
                    val ts = ISODateTimeFormat.dateTime().print(dt)
                    val j = Json.obj("status"->"success","event"->Map("eventtype" -> "x10", "ts" -> ts, "code" -> pendingCodeAndUnit.get, "function" -> items(4).toLowerCase()))
                    Global.eventManager.processEvent(j.toString())
                    //pendingCodeAndUnit = None
                  }
                  else {
                    logger.warn("received func x10 message yet have no pendingCodeAndUnit")
                  }
                }
              }
            }
            catch {
              case (e: Exception) => logger.error("caught exception processing monitor output", e)
            }
          })
        }
        catch {
          case (e: InterruptedException) => logger.info("shutting down monitor listener")
          case (e: Exception) => logger.error("caught exception in monitor run loop", e)
        }
      }
    })
    listenthread.start()
  }


  def run(): Unit = {
    var breakout = false

    while (!breakout) {
      try {
        val r = queue.poll(60, TimeUnit.SECONDS)
        if (r != null) {
          val cmd = s"$heyu ${
            r.function
          } ${
            r.houseCode
          }${
            r.unitCode
          }"
          logger.debug(s"exec $cmd")
          val output = cmd.!!
          //logger.debug(s"command output: $output")
          r.p.success(output)
        }
      }
      catch {
        case (ie: InterruptedException) =>
          logger.info("thread interrupted.  Returning from run loop")
          breakout = true
        case (e: Exception) =>
          logger.error("exception caught in run loop", e)
          breakout = true
      }
    }
  }

  def getState: List[String] = {
    val infop = "  Housecode ([A-P]) \\(([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])([.|*])\\)".r
    val infop2 = "  Housecode ([A-P]) \\(([.|*]+)\\)".r

    val cmd = s"$heyu show h"
    logger.debug(s"exec $cmd")
    val output = cmd.!!
    output.split('\n').flatMap {
      case infop2(house, units) =>
        val x = units.zipWithIndex.map {
          case (('*', i)) => List(house + (i+1).toString)
          case _ => List()
        }.toList
        x.flatten
      //= "  Housecode A (.*.*............)"
      case _ => List()
    }.toList
  }

  def x10(houseCodeUnit: String, function: String, arg: Option[String]): Future[String] = {
    val p = Promise[String]()
    queue.add(Request(houseCodeUnit.charAt(0), houseCodeUnit.charAt(1), function, p, arg))
    p.future
  }

  def shutdown(): Unit = {
    try {
      if (thread != null) {
        thread.interrupt()
      }
      if (listenthread != null) {
        listenthread.interrupt()
      }
    }
    catch {
      case (e: Exception) => logger.error("shutdown of serial port failed", e)
    }
  }
}
