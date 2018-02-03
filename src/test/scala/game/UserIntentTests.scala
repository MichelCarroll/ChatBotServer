
package game

import org.scalatest._

class UserIntentTests extends FlatSpec with Matchers {

  def test(text: String, expectedUserIntents: Option[UserIntent]): Assertion = {
    val userIntent = UserIntentExtractor.userIntent(text)
    userIntent should equal(expectedUserIntents)
  }

  implicit class TextPimp(text: String) {
    def means(userIntent: UserIntent) =
      text should s"map to $userIntent" in {
        test(text, Some(userIntent))
      }
  }

  "wallet" means AskWallet
  "how much money do I have?" means AskWallet
  "how much gold is in my wallet?" means AskWallet
  "how much money is in my wallet?" means AskWallet
  "how rich am I?" means AskWallet
  "how wealthy am I?" means AskWallet
  "how much wealth do I have?" means AskWallet

  "inventory" means AskWholeInventory
  "what's in my inventory?" means AskWholeInventory
  "how much stuff do I have?" means AskWholeInventory
  "how many commodities do I have?" means AskWholeInventory
  "how big is my inventory?" means AskWholeInventory
  "what do I have?" means AskWholeInventory
  "what's the state of my inventory?" means AskWholeInventory
  "how much do I have in stock?" means AskWholeInventory
  "what do I have in stock?" means AskWholeInventory

  "how many lemons do I have?" means AskCommodityInventory(Lemon)
  "how much lemons do I have?" means AskCommodityInventory(Lemon)
  "what's my lemon situation?" means AskCommodityInventory(Lemon)
  "lemons" means AskCommodityInventory(Lemon)
  "how many lemons" means AskCommodityInventory(Lemon)
  "how many lemons are in my inventory?" means AskCommodityInventory(Lemon)
  "what's my lemon inventory?" means AskCommodityInventory(Lemon)
  "lemon inventory" means AskCommodityInventory(Lemon)
  "lemon state" means AskCommodityInventory(Lemon)
  "lemons in stock" means AskCommodityInventory(Lemon)

  "can i buy lemons?" means AskIfAbleToBuyCommodity(Quantity(1),Lemon)
  "can I buy a lemon?" means AskIfAbleToBuyCommodity(Quantity(1),Lemon)
  "can I buy two lemons?" means AskIfAbleToBuyCommodity(Quantity(2),Lemon)
  "can I afford a lemon?" means AskIfAbleToBuyCommodity(Quantity(1),Lemon)
  "could I buy one lemon?" means AskIfAbleToBuyCommodity(Quantity(1),Lemon)

  "how many lemons can I buy?" means AskMaxCommodityCanBuy(Lemon)
  "how many lemons can I afford?" means AskMaxCommodityCanBuy(Lemon)
  "max lemons I can buy" means AskMaxCommodityCanBuy(Lemon)
  "max lemons I can afford" means AskMaxCommodityCanBuy(Lemon)
  "maximum lemons I can buy" means AskMaxCommodityCanBuy(Lemon)
  "maximum amount of lemons I can afford" means AskMaxCommodityCanBuy(Lemon)
  "how many lemons do I have enough money for?" means AskMaxCommodityCanBuy(Lemon)

  "how much money would I make selling all my lemons?" means AskGoldForSellingMaxCommodity(Lemon)
  "how much money would I get back from selling my lemons?" means AskGoldForSellingMaxCommodity(Lemon)
  "max money I would make selling lemons" means AskGoldForSellingMaxCommodity(Lemon)
  "how much are my lemons worth?" means AskGoldForSellingMaxCommodity(Lemon)
  "how much is my lemon inventory worth?" means AskGoldForSellingMaxCommodity(Lemon)

  "how much money would I Make from selling two lemons?" means AskGoldForSellingCommodity(Quantity(2),Lemon)
  "how much money for selling three lemons?" means AskGoldForSellingCommodity(Quantity(3),Lemon)
  "how much would I make for selling a lemon?" means AskGoldForSellingCommodity(Quantity(1),Lemon)
  "how much gold would I get from selling a lemon?" means AskGoldForSellingCommodity(Quantity(1),Lemon)
  "how much would I get back from selling two lemons?" means AskGoldForSellingCommodity(Quantity(2),Lemon)

  "how much are lemons worth?" means AskMarketPriceCommodity(Lemon)
  "what's the value of lemons?" means AskMarketPriceCommodity(Lemon)
  "lemon value" means AskMarketPriceCommodity(Lemon)
  "lemon market worth" means AskMarketPriceCommodity(Lemon)
  "how much are lemons going for?" means AskMarketPriceCommodity(Lemon)

  "sell a lemon" means SellCommodity(Quantity(1),Lemon)
  "sell two lemons" means SellCommodity(Quantity(2),Lemon)
  "sell lemon" means SellCommodity(Quantity(1),Lemon)
  "sell 10 lemons" means SellCommodity(Quantity(10),Lemon)

  "buy a lemon" means BuyCommodity(Quantity(1),Lemon)
  "buy 2 lemons" means BuyCommodity(Quantity(2),Lemon)
  "buy lemon" means BuyCommodity(Quantity(1),Lemon)
  "purchase a lemon" means BuyCommodity(Quantity(1),Lemon)


}