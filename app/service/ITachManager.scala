package service

import java.io._
import java.net.{ConnectException, Socket}
import java.util.concurrent.{ExecutorService, Executors}

import org.slf4j.LoggerFactory
import play.api.Play
import play.api.libs.concurrent.Akka
import scala.concurrent.{Promise, Future, ExecutionContext}
import scala.concurrent.duration._
import play.api.Play.current

import scala.util.Random

/**
 * Manage connection to ITach IR2IP device
 */
class ITachManager {
  val logger = LoggerFactory.getLogger(classOf[ITachManager])
  val pool: ExecutorService = Executors.newFixedThreadPool(5)
  val cfg = Play.current.configuration
  var sock: Option[Socket] = None
  var socketOutputStream: OutputStream = _
  var outstandingPromise: Option[Promise[String]] = None

  implicit val ec = ExecutionContext.fromExecutorService(pool)

  def init(): Unit = {
    tryToConnect(pool)
  }

  def shutdown(): Unit = {
    sock match {
      case Some(s) =>s.close()
      case None => // nothing
    }
  }

  def sendIr(where: String, key: String): Future[String] = {
    val p = Promise[String]()
    try {
      val rnd = Random.nextInt(65535)
      val addr = cfg.getString(s"ir.address.$where").getOrElse("1:3")
      cfg.getString(s"ir.$key") match {
        case Some(v) =>
          val cmd = s"sendir,$addr,$rnd,$v\r"
          logger.debug(s"sending $cmd")
          socketOutputStream.write(cmd.getBytes)
          outstandingPromise = Some(p)
        case None =>
          logger.error(s"no ir code found matching key $key")
          p.failure(new IllegalArgumentException(s"bad key $key"))
      }
    }
    catch {
      case (e: Exception) => logger.error("could not send due to exception", e)
    }

    p.future
  }

  def getNetworkConfig: Future[String] = {
    val p = Promise[String]()
    try {
      val cmd = "get_NET,0:1\r"
      socketOutputStream.write(cmd.getBytes)
      outstandingPromise = Some(p)
    }
    catch {
      case (e: Exception) => logger.error("could not send due to exception", e)
    }

    p.future
  }

  class SocketListener(socket:Socket) extends Runnable {
    def run(): Unit = {
      var cont = true
      while (cont) {
        val in = socket.getInputStream
        val isr = new BufferedReader(new InputStreamReader(in))
        try {
          val input = isr.readLine()
          outstandingPromise match {
            case Some(p) => p.success(input); outstandingPromise = None
            case None =>
              logger.debug(s"received message $input")
          }
        }
        catch {
          case (e: IOException) =>
            logger.error("failed to read from socket stream", e)
            cont = false
        }
      }
    }
  }

  def tryToConnect(pool:ExecutorService): Unit = {
    val host = cfg.getString("itach.host").get
    val port = cfg.getInt("itach.port").getOrElse(4988)
    try {
      logger.debug(s"attempting connection to $host $port")
      sock = Some(new Socket(host, port))
      socketOutputStream = sock.get.getOutputStream
      logger.debug(s"connected to $host $port")
      pool.execute(new SocketListener(sock.get))
    }
    catch {
      case (e: ConnectException) =>
        logger.debug("unable to connect.  will try again")

        Akka.system.scheduler.scheduleOnce(30.seconds) {
          tryToConnect(pool)
        }
      case (e: Exception) => logger.error("failed in connection request", e)
    }
  }

}
