package pattern

import org.apache.logging.log4j.scala.Logging
import org.scalatest._

class AnonymousFunctionSpec extends FlatSpec with Logging  {

  val wordFrequencies = ("habitual", 6) :: ("and", 56) :: ("consuetudinary", 2) ::
    ("additionally", 27) :: ("homely", 5) :: ("society", 13) :: Nil

//  def wordsWithoutOutliers(wordFrequencies: Seq[(String, Int)]): Seq[String] =
//    wordFrequencies.filter(wf => wf._2 > 3 && wf._2 < 25).map(_._1)
//  wordsWithoutOutliers(wordFrequencies) // List("habitual", "homely", "society")

  def wordsWithoutOutliers(wordFrequencies: Seq[(String, Int)]): Seq[String] =
    wordFrequencies.filter {
      case (_, f) => f > 3 && f < 25
    } map {
      case (w, _) => w
    }

  val predicate: ((String, Int)) => Boolean = { case (_, f) => f > 3 && f < 25 }
  val transformer: ((String, Int)) => String = { case (w, _) => w }

  val pf: PartialFunction[(String, Int), String] = {
    case (word, freq) if freq > 3 && freq < 25 => word
  }

  "anonymous functions" should "be easy" in {
    logger.info("result: "+wordsWithoutOutliers(wordFrequencies))
    logger.info("result: "+wordFrequencies.filter(predicate).map(transformer))
    try {
      logger.info("result: "+wordFrequencies.map(pf))
    } catch {
      case t:MatchError => logger.info("expected this Match Error: "+t)
    }
    logger.info("wordFrequencies.collect(pf) will work: "+wordFrequencies.collect(pf))
  }


}
