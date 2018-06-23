package pattern

import org.apache.logging.log4j.scala.Logging
import org.scalatest._

class PatternsEverywhereSpec extends FlatSpec with Logging  {

  case class Player(name: String, score: Int)

  def printMessage(player: Player) = logger.info("message for player "+player+" is "+message(player))

  def message(player: Player) = player match {
    case Player(_, score) if score > 100000 => "Get a job, dude!"
    case Player(name, _) => "Hey " + name + ", nice to see you again!"
  }

  "patterns " should "be everywhere" in {
    printMessage(Player("Damian", 25000))
  }

  def currentPlayer(): Player = Player("Daniel", 3500)
  val Player(name, _) = currentPlayer()
  logger.info("name is "+name)

  "match statements" should "be exhaustive" in {
    try {
      def scores: List[Int] = List()
      val best :: rest = scores
      logger.info("The score of our champion is " + best) // never reached
    } catch {
      case t:MatchError => logger.info("we expect a match error here: "+t)
    }
  }

  "pattern match" should "be used to unpack tuples" in {
    def gameResult(player: Player) = (player.name, player.score)
    val (name, score) = gameResult(currentPlayer())
    logger.info("name:score is "+name + ": " + score)
  }

  def gameResults(): Seq[(String, Int)] =
    ("Daniel", 3500) :: ("Melissa", 13000) :: ("John", 7000) :: Nil

  def hallOfFame = for {
//    result <- gameResults()
//    (name, score) = result
    (name, score) <- gameResults()
    if (score > 5000)
  } yield name

  "for comprehensions" should "be syntactic sugar" in {
    logger.info("hallOfFame returns: "+hallOfFame)
  }

  val lists = List(1, 2, 3) :: List.empty :: List(5, 3) :: Nil


  "for comprehensions" should "be used to get the sizes of non-emply lists" in {
    val result = for {
      list @ head :: _ <- lists // the pattern on the LHS will not match the empty list, thus filtering it
    } yield list.size
    logger.info("result: "+result)

    // this is equivalent to:
    logger.info("this is equivalent to: "+lists.filter(l => l.size > 0).map(l => l.size).foldLeft[List[Int]](List.empty[Int])((l, i) => i :: l))
  }
}
