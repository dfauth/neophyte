package extractors

import org.apache.logging.log4j.scala.Logging
import org.scalatest._

class SequenceExtractorsSpec extends FlatSpec with Logging  {

  "a stream" should "evaluate lazily" in {
    val xs = 3 :: 6 :: 12 :: 24 :: Nil
    val result = xs match {
      case List(a, b) => a + b // 9
      case List(a, b, c) => a + b + c // 21
      case List(a, b, _*) => a * b // 18
      case _ => 0
    }
    logger.info("result: "+result)
  }

  object GivenNames {
    def unapplySeq(name: String): Option[Seq[String]] = {
      val names = name.trim.split(" ")
      if (names.forall(_.isEmpty)) None else Some(names)
    }
  }

  def greetWithFirstName(name: String) = name match {
    case GivenNames(firstName, _*) => "Good morning, " + firstName + "!"
    case _ => "Welcome! Please make sure to fill in your name!"
  }

  "extract a " should "sequence" in {
    logger.info("greetWithFirstName(\"Daniel\") returns: "+greetWithFirstName("Daniel"))
    logger.info("greetWithFirstName(\"Catherina Johanna\") returns: "+greetWithFirstName("Catherina Johanna"))
  }

  object Names {
    def unapplySeq(name: String): Option[(String, String, Seq[String])] = {
      val names = name.trim.split(" ")
      if (names.size < 2) None
      else Some((names.last, names.head, names.drop(1).dropRight(1)))
    }
  }

  def greet(fullName: String) = fullName match {
    case Names(lastName, firstName, _*) => "Good morning, " + firstName + " " + lastName + "!"
    case _ => "Welcome! Please make sure to fill in your name!"
  }

  "greet" should "matches a tuple of names" in {
    logger.info("greet(\"John Doe\") returns: "+greet("John Doe"))
    logger.info("greet(\"Catherina Johanna Peterson\") returns: "+greet("Catherina Johanna Peterson"))
  }

}
