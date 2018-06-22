package extractors

import org.apache.logging.log4j.scala.Logging
import org.scalatest._

class ExtractorsSpec extends FlatSpec with Logging  {

  case class SimpleUser(firstName:String, lastName:String, score:Int)

  def advance(xs:List[SimpleUser]) = xs match {
    case SimpleUser(_,_, score1) :: SimpleUser(_,_, score2) :: _ => score1 - score2
    case _ => 0
  }

  trait User {
    def name:String
    def score:Int
  }

  case class FreeUser(val name:String, val score:Int, val upgradeProbablity:Double) extends User
  case class PremiumUser(val name:String, val score:Int) extends User

  object FreeUser {
    def unapply(user:FreeUser):Option[(String, Int, Double)] = Some((user.name, user.score, user.upgradeProbablity))
  }

  object PremiumUser {
    def unapply(user:PremiumUser):Option[(String, Int)] = Some((user.name, user.score))
  }

  object PremiumCantidate {
    def unapply(user:FreeUser):Boolean = user.upgradeProbablity > 0.75
  }

  object premiumCandidate {
    def unapply(user: FreeUser): Boolean = user.upgradeProbablity > 0.75
  }

  "blah " should " return blah" in {
    val testList = List(SimpleUser("fred", "flintstone", 40), SimpleUser("wilma", "flintstone", 36), SimpleUser("barney", "flintstone", 39))

    val result = advance(testList)
    logger.info("result: "+result)
  }

  "user unapply" should "like a boss" in {
    val user:User = new PremiumUser("daniel", 3000)
    def greeting(user:User):String = user match {
      case FreeUser(name, _, p) => if(p > 0.75) name + " what can we do for you today?" else "Hello " + name
      case PremiumUser(name, _) => "Welcome back, dear " + name
    }
    logger.info("greeting: "+greeting(user))
    logger.info("greeting: "+greeting(FreeUser("damian", 36, 0.74d)))
    logger.info("greeting: "+greeting(FreeUser("fred", 46, 0.90d)))

    val user1:User = FreeUser("daniel", 2500, 0.8d)
  }

  "boolean unapply" should "be something" in {
    val user: User = new FreeUser("Daniel", 2500, 0.8d)
    user match {
      case freeUser @ premiumCandidate() => logger.info("premiumCandidate: "+freeUser)
      case _ => logger.info("default")
    }
  }

  "a stream" should "evaluate lazily" in {
    val xs = 58 #:: 43 #:: 93 #:: Stream.empty
    val result = xs match {
//      case first #:: second #:: _ => first - second // head #:: tail == #::(head,tail)
      case #::(first, #::(second, _)) => first - second
      case _ => -1
    }
    logger.info("result: "+result)
  }
}
