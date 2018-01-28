
import game._
import net.didion.jwnl.data.POS
import nlp._
import org.scalatest._
import sext._
import wordnet.WordNet


class Test extends FlatSpec with Matchers {

  val parseTreeBuilder = new Annotator()
  implicit val wordNet = WordNet()
  val userIntentTester = new UserIntentTester()

  val tests = Map[String, List[UserIntent]](
    "buy two lemons" -> List(BuyCommodity(Quantity(2),Lemon)),
    "buy a lemon" -> List(BuyCommodity(Quantity(1),Lemon)),
    "buy the lemon" -> List(),
    "I want to buy two lemons" -> List(BuyCommodity(Quantity(2),Lemon)),
    "buy a lemon" -> List(BuyCommodity(Quantity(1),Lemon)),
    "buy two dozens of lemons" -> List(BuyCommodity(Quantity(2, Dozen),Lemon)),
    "buy two lemon dozens" -> List(BuyCommodity(Quantity(2, Dozen),Lemon)),
    "buy two cups" -> List(BuyCommodity(Quantity(2),Cup)),
    "buy a cup" -> List(BuyCommodity(Quantity(1),Cup)),
    "buy the cup" -> List(),
    "I want to buy two cups" -> List(BuyCommodity(Quantity(2),Cup)),
    "purchase a cup" -> List(BuyCommodity(Quantity(1),Cup)),
  )

  "something" should "something" in {
    tests.foreach { case (key, expectedOutput) =>
      val trees = parseTreeBuilder.build(key)
      val userIntents = trees.flatMap(userIntentTester.testUserIntent)
      userIntents should equal(expectedOutput)
    }
  }

}