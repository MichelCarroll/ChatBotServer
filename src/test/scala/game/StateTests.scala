
package game

import org.scalatest._

class StateTests extends FlatSpec with Matchers {

  val playerId = PlayerId("p1")

  def stateWithGold(gold: Gold) =
    GameState(marketState = MarketState.initial, playerStates = Map(playerId -> PlayerState(gold, PlayerState.initialInventory)))

  def stateWithInventory(inventory: Inventory) =
    GameState(marketState = MarketState.initial, playerStates = Map(playerId -> PlayerState.initial.copy(inventory = inventory)))

  def stateWithLemonQuantity(quantity: Quantity) =
    stateWithInventory(PlayerState.initialInventory.copy(commodities =
      PlayerState.initialInventory.commodities.updated(Lemon, quantity)
    ))

  "AskWallet" should s"return 0 gold if the wallet is empty" in {
    val initial = stateWithGold(Gold(0))
    val (_, notification) = initial.execute(playerId, AskWallet)
    notification should equal(AnnounceGoldOwned(Gold(0)))
  }

  it should s"return 100 gold if the wallet has 100 gold" in {
    val initial = stateWithGold(Gold(100))
    val (_, notification) = initial.execute(playerId, AskWallet)
    notification should equal(AnnounceGoldOwned(Gold(100)))
  }

  "SellCommodity" should s"not work if inventory empty" in {
    val initial = stateWithLemonQuantity(Quantity(0))
    val (_, notification) = initial.execute(playerId, SellCommodity(Quantity(1), Lemon))
    notification should equal(AnnounceNotEnoughInventory)
  }

  it should s"not work if inventory doesn't have enough in it" in {
    val initial = stateWithLemonQuantity(Quantity(1))
    val (_, notification) = initial.execute(playerId, SellCommodity(Quantity(5), Lemon))
    notification should equal(AnnounceNotEnoughInventory)
  }

  it should s"work if inventory has the same amount" in {
    val initial = stateWithLemonQuantity(Quantity(10))
    val (newState, notification) = initial.execute(playerId, SellCommodity(Quantity(10), Lemon))
    notification should equal(AnnounceSuccessfulSell(Lemon, Quantity(0)))
    newState.playerStates(playerId).inventory.commodities(Lemon) should equal(Quantity(0))
  }

  it should s"work if inventory has more than needed" in {
    val initial = stateWithLemonQuantity(Quantity(15))
    val (newState, notification) = initial.execute(playerId, SellCommodity(Quantity(10), Lemon))
    notification should equal(AnnounceSuccessfulSell(Lemon, Quantity(5)))
    newState.playerStates(playerId).inventory.commodities(Lemon) should equal(Quantity(5))
  }

}

/**

case object AskWallet extends UserIntent
case object AskWholeInventory extends UserIntent
case class AskCommodityInventory(commodity: Commodity) extends UserIntent
case class AskIfAbleToBuyCommodity(quantity: Quantity, commodity: Commodity) extends UserIntent
case class AskMarketPriceCommodity(commodity: Commodity) extends UserIntent
case class AskMaxCommodityCanBuy(commodity: Commodity) extends UserIntent
case class AskGoldForSellingCommodity(quantity: Quantity, commodity: Commodity) extends UserIntent
case class AskGoldForSellingMaxCommodity(commodity: Commodity) extends UserIntent
case class SellCommodity(quantity: Quantity, commodity: Commodity) extends UserIntent
case class BuyCommodity(quantity: Quantity, commodity: Commodity) extends UserIntent
  */