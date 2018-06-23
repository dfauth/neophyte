package option

import org.apache.logging.log4j.scala.Logging
import org.scalatest._

class OptionSpec extends FlatSpec with Logging  {

  case class User(
                   id: Int,
                   firstName: String,
                   lastName: String,
                   age: Int,
                   gender: Option[String])

  object UserRepository {
    private val users = Map(1 -> User(1, "John", "Doe", 32, Some("male")),
      2 -> User(2, "Johanna", "Doe", 30, None))
    def findById(id: Int): Option[User] = users.get(id)
    def findAll = users.values
  }

  "option" should "be easy" in {
    val user = UserRepository.findById(2)
    val gender = user match {
      case Some(user) => user.gender
      case None => "not specified"
    }
    logger.info("Gender: " + gender)

    val age = UserRepository.findById(1).map(_.age) // age is Some(32)
    logger.info("age for id 1: " + age)

    logger.info("UserRepository.findById(1).map(_.gender): "+UserRepository.findById(1).map(_.gender)) // gender is an Option[Option[String]]
    logger.info("UserRepository.findById(1).flatMap(_.gender): "+
        UserRepository.findById(1).flatMap(_.gender) // gender is Some("male")
    ) // gender is an Option[String]
    logger.info("UserRepository.findById(2).flatMap(_.gender): "+
      UserRepository.findById(2).flatMap(_.gender) // gender is None
    ) // gender is an Option[String]
    logger.info("UserRepository.findById(3).flatMap(_.gender): "+
      UserRepository.findById(3).flatMap(_.gender) // gender is None
    ) // gender is an Option[String]

    val names: List[List[String]] =
      List(List("John", "Johanna", "Daniel"), List(), List("Doe", "Westheide"))
    logger.info("names.map: "+names.map(_.map(_.toUpperCase)))
    // results in List(List("JOHN", "JOHANNA", "DANIEL"), List(), List("DOE", "WESTHEIDE"))
    logger.info("names.flatMap: "+names.flatMap(_.map(_.toUpperCase)))
    // results in List("JOHN", "JOHANNA", "DANIEL", "DOE", "WESTHEIDE")

    val moreNames: List[Option[String]] = List(Some("Johanna"), None, Some("Daniel"))
    logger.info("moreNames.map: "+moreNames.map(_.map(_.toUpperCase))) // List(Some("JOHANNA"), None, Some("DANIEL"))
    logger.info("moreNames.flatMap: "+moreNames.flatMap(xs => xs.map(_.toUpperCase))) // List("JOHANNA", "DANIEL")

    logger.info("UserRepository.findById(1).filter(_.age > 30): "+UserRepository.findById(1).filter(_.age > 30)) // Some(user), because age is > 30
    logger.info("UserRepository.findById(2).filter(_.age > 30): "+UserRepository.findById(2).filter(_.age > 30)) // None, because age is <= 30
    logger.info("UserRepository.findById(3).filter(_.age > 30): "+UserRepository.findById(3).filter(_.age > 30)) // None, because user is already None

    UserRepository.findAll.filter(_.age > 30).foreach(a=>logger.info("users: "+a))

    def findGenderById(id:Int):Option[String] = {
      for {
        user <- UserRepository.findById(id)
        gender <- user.gender
      } yield gender // results in Some("male")
    }
    logger.info("user 1 gender: "+findGenderById(1).getOrElse("not found"))
    logger.info("user 2 gender: "+findGenderById(2).getOrElse("not found"))

    val allGenders = for {
      user <- UserRepository.findAll
      gender <- user.gender
    } yield gender
    logger.info("all genders: "+allGenders)

    val allGenders1 = for {
      User(_, _, _, _, Some(gender)) <- UserRepository.findAll
    } yield gender
    logger.info("all genders: "+allGenders1)

    case class Resource(content: String)
    val resourceFromConfigDir: Option[Resource] = None
    val resourceFromClasspath: Option[Resource] = Some(Resource("I was found on the classpath"))
    val resource = resourceFromConfigDir orElse resourceFromClasspath
    logger.info("resource: "+resource)
  }

}
