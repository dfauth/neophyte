package whytry

import java.io.FileNotFoundException
import java.net.MalformedURLException

import org.apache.logging.log4j.scala.Logging
import org.scalatest._

import scala.util.{Failure, Success}

class TrySpec extends FlatSpec with Logging  {

  case class Customer(age: Int)
  class Cigarettes
  case class UnderAgeException(message: String) extends Exception(message)
  def buyCigarettes(customer: Customer): Cigarettes =
    if (customer.age < 16)
      throw UnderAgeException(s"Customer must be older than 16 but was ${customer.age}")
    else new Cigarettes

  "you can throw and catch excpetions in " should "scala" in {
    val youngCustomer = Customer(15)
    val result = try {
      buyCigarettes(youngCustomer)
      "Yo, here are your cancer sticks! Happy smokin'!"
    } catch {
      case UnderAgeException(msg) => msg
    }
    logger.info("result: "+result)
  }

  import scala.util.Try
  import java.net.URL
  def parseURL(url: String): Try[URL] = Try(new URL(url))

  "try this using" should "URL" in {
    logger.info("parseURL(\"http://danielwestheide.com\"): "+parseURL("http://danielwestheide.com"))
    logger.info("parseURL(\"sdgdgdf\"): "+parseURL("sdgdgdf"))

    val result = parseURL("http://danielwestheide.com").map(_.getProtocol)
    // results in Success("http")
    logger.info("result: "+result)
    val result1 = parseURL("garbage").map(_.getProtocol)
    // results in Failure(java.net.MalformedURLException: no protocol: garbage)
    logger.info("result1: "+result1)
  }

  import java.io.InputStream
  def inputStreamForURL(url: String): Try[InputStream] = parseURL(url).flatMap { u =>
    Try(u.openConnection()).flatMap(conn => Try(conn.getInputStream))
  }

  def parseHttpURL(url: String) = parseURL(url).filter(_.getProtocol == "http")
  parseHttpURL("http://apache.openmirror.de") // results in a Success[URL]
  parseHttpURL("ftp://mirror.netcologne.de/apache.org") // results in a Failure[URL]

  "try inputStreamForURL" should "work" in {
    logger.info("result: "+inputStreamForURL("http://danielwestheide.com"))
    logger.info("result: "+inputStreamForURL("blah!"))
    logger.info("parseHttpURL(\"http://apache.openmirror.de\"): "+parseHttpURL("http://apache.openmirror.de"))
    logger.info("parseHttpURL(\"ftp://mirror.netcologne.de/apache.org\"): "+parseHttpURL("ftp://mirror.netcologne.de/apache.org"))
  }

  import scala.io.Source
  def getURLContent(url: String): Try[Iterator[String]] =
    for {
      url <- parseURL(url)
      connection <- Try(url.openConnection())
      is <- Try(connection.getInputStream)
      source = Source.fromInputStream(is)
    } yield source.getLines()

  "try in for comprehensions" should "be sexy" in {
    getURLContent("http://danielwestheide.com/foobar") match {
      case Success(lines) => lines.foreach(l => logger.info(l))
      case Failure(ex) => logger.info(s"Problem rendering URL content: ${ex.getMessage}")
    }

    val content = getURLContent("garbage") recover {
      case e: FileNotFoundException => Iterator("Requested page does not exist")
      case e: MalformedURLException => Iterator("Please make sure to enter a valid URL")
      case _ => Iterator("An unexpected error has occurred. We are so sorry!")
    }
    content.map[Try[Iterator[String]]](s => Try(s)) match {
      case Success(it) => it.foreach(l => logger.info("content: "+l))
      case Failure(oops) => logger.info("oopsy: "+oops)
    }
  }
}
