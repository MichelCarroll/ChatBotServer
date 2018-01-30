
package game

import nlp._
import org.scalatest._
import wordnet.WordNet

class UserIntentTests extends FlatSpec with Matchers {

  def test(text: String, expectedUserIntents: Option[UserIntent]): Assertion = {
    val userIntents = UserIntentTester.testUserIntent(text)
    userIntents should equal(expectedUserIntents)
  }

  "lemon buying" should "map to the right intent" in {
    test("can I buy two lemons", Some(BuyCommodity(Quantity(2),Lemon)))
    test("buy two lemons", Some(BuyCommodity(Quantity(2),Lemon)))
    test("buy a lemon", Some(BuyCommodity(Quantity(1),Lemon)))
    test("buy lemon", Some(BuyCommodity(Quantity(1),Lemon)))
    test("purchase two lemons", Some(BuyCommodity(Quantity(2),Lemon)))
    test("purchase a lemon", Some(BuyCommodity(Quantity(1),Lemon)))
    test("purchase lemon", Some(BuyCommodity(Quantity(1),Lemon)))
    test("buy two watermelons", Some(BuyCommodity(Quantity(2),Watermelon)))
    test("buy a watermelon", Some(BuyCommodity(Quantity(1),Watermelon)))
    test("buy watermelon", Some(BuyCommodity(Quantity(1),Watermelon)))
    test("purchase two watermelons", Some(BuyCommodity(Quantity(2),Watermelon)))
    test("purchase a watermelon", Some(BuyCommodity(Quantity(1),Watermelon)))
    test("purchase watermelon", Some(BuyCommodity(Quantity(1),Watermelon)))
  }

//  val tests = Map[String, List[UserIntent]](
//    "buy two lemons" -> List(BuyCommodity(Quantity(2),Lemon)),
//    "buy a lemon" -> List(BuyCommodity(Quantity(1),Lemon)),
//    "buy the lemon" -> List(),
//    "I want to buy two lemons" -> List(BuyCommodity(Quantity(2),Lemon)),
//    "buy a lemon" -> List(BuyCommodity(Quantity(1),Lemon)),
//    "buy two dozens of lemons" -> List(BuyCommodity(Quantity(2, Dozen),Lemon)),
//    "buy two lemon dozens" -> List(BuyCommodity(Quantity(2, Dozen),Lemon)),
//    "buy two cups" -> List(BuyCommodity(Quantity(2),Cup)),
//    "buy a cup" -> List(BuyCommodity(Quantity(1),Cup)),
//    "buy the cup" -> List(),
//    "I want to buy two cups" -> List(BuyCommodity(Quantity(2),Cup)),
//    "purchase a cup" -> List(BuyCommodity(Quantity(1),Cup)),
//  )

}