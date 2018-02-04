package game

sealed trait InvalidCommand
case object InsufficientMoney extends InvalidCommand
case object InsufficientInventory extends InvalidCommand

case class MarketState(prices: Map[Commodity, Gold])
object MarketState {
  def initial = MarketState(
    Commodity.all.map(c => c -> c.initialValue).toMap
  )
}

case class Inventory(commodities: Map[Commodity, Quantity])

case class PlayerId(id: String) extends AnyVal
case class PlayerState(gold: Gold, inventory: Inventory) {

  def buy(commodity: Commodity, quantity: Quantity, marketState: MarketState): Either[InvalidCommand, PlayerState] = {
    val cost = marketState.prices(commodity) * quantity
    if(gold >= cost)
      Right(
        copy(
          gold = gold - cost,
          inventory = inventory.copy(commodities =
            inventory.commodities.updated(commodity, inventory.commodities(commodity) + quantity)
          )
        )
      )
    else
      Left(InsufficientMoney)
  }

  def sell(commodity: Commodity, quantity: Quantity, marketState: MarketState): Either[InvalidCommand, PlayerState] = {
    if(inventory.commodities(commodity) >= quantity)
      Right(
        copy(
          gold = gold + marketState.prices(commodity) * quantity,
          inventory = inventory.copy(commodities =
            inventory.commodities.updated(commodity, inventory.commodities(commodity) - quantity)
          )
        )
      )
    else
      Left(InsufficientInventory)
  }

}

object PlayerState {
  def initialInventory: Inventory = Inventory(Commodity.all.map((c: Commodity) => c -> Quantity(0)).toMap)
  def initial = PlayerState(Gold(100), initialInventory)
}

trait Vulgarizations {
  implicit class CommodityPimps(commodity: Commodity) {
    def vulgarizedWithQuantity(quantity: Quantity): String = quantity match {
      case Quantity(0) => s"no ${commodity.plural}"
      case Quantity(1) => s"1 ${commodity.singular}"
      case Quantity(q) => s"$q ${commodity.plural}"
    }
  }

  implicit class InventoryPimp(inventory: Inventory) {
    def vulgarizedInventory: List[(Commodity, String)] = inventory.commodities
      .toList
      .sortBy(_._2.amount)
      .reverse
      .map { case (c, _) => (
        c,
        c.vulgarizedWithQuantity(inventory.commodities(c))
      ) }
  }
}

sealed trait Notification {
  def text: String
}

case class AnnounceGoldOwned(gold: Gold) extends Notification {
  def text = s"You have ${gold.amount} gold."
}

case class AnnounceWholeInventory(inventory: Inventory) extends Notification with Vulgarizations {
  def text = s"You have ${inventory.vulgarizedInventory.map(_._2).mkString(", ")}."
}

case class AnnounceCommodityInventory(commodity: Commodity, quantity: Quantity) extends Notification with Vulgarizations {
  def text = s"You have ${commodity.vulgarizedWithQuantity(quantity)}."
}

case class AnnounceCanAfford(commodity: Commodity, quantity: Quantity) extends Notification with Vulgarizations {
  def text = s"Yes, you can afford ${commodity.vulgarizedWithQuantity(quantity)}."
}

case class AnnounceCannotAfford(commodity: Commodity, quantity: Quantity) extends Notification with Vulgarizations {
  def text = s"No, you can't afford ${commodity.vulgarizedWithQuantity(quantity)}."
}

case class AnnounceCommodityMarketPrice(commodity: Commodity, gold: Gold) extends Notification {
  def text = s"The ${commodity.plural} are worth ${gold.amount} gold."
}

case class AnnounceMaxCommodityCanAfford(commodity: Commodity, quantity: Quantity) extends Notification with Vulgarizations {
  def text = s"You can afford ${commodity.vulgarizedWithQuantity(quantity)}."
}

case class AnnounceGoldFromSellingCommodity(gold: Gold, commodity: Commodity, quantity: Quantity) extends Notification with Vulgarizations {
  def text = s"You would make ${gold.amount} gold for selling ${commodity.vulgarizedWithQuantity(quantity)}."
}

case class AnnounceGoldFromSellingMaxCommodity(gold: Gold, commodity: Commodity, quantity: Quantity) extends Notification with Vulgarizations {
  def text = s"You would make ${gold.amount} gold for selling ${commodity.vulgarizedWithQuantity(quantity)}."
}

case class AnnounceSuccessfulBuy(commodity: Commodity, quantity: Quantity) extends Notification with Vulgarizations {
  def text = s"OK! You now have ${commodity.vulgarizedWithQuantity(quantity)}."
}

case class AnnounceSuccessfulSell(commodity: Commodity, quantity: Quantity) extends Notification with Vulgarizations {
  def text = s"OK! You now have ${commodity.vulgarizedWithQuantity(quantity)}."
}

case object AnnounceNotEnoughGold extends Notification {
  def text = "You don't have enough gold!"
}

case object AnnounceNotEnoughInventory extends Notification {
  def text = "You don't have enough in your inventory!"
}

case class GameState(
                    playerStates: Map[PlayerId, PlayerState],
                    marketState: MarketState
                    ) {

  implicit class InvalidCommandPimp(invalidCommand: InvalidCommand) {
    def notification: Notification = invalidCommand match {
      case InsufficientInventory => AnnounceNotEnoughInventory
      case InsufficientMoney => AnnounceNotEnoughGold
    }
  }

  def join(playerId: PlayerId): GameState =
    copy(playerStates.updated(playerId, PlayerState.initial))

  def execute(playerId: PlayerId, userIntent: UserIntent): (GameState, Notification) =
    userIntent match {
      case AskWallet =>
        (this, AnnounceGoldOwned(playerStates(playerId).gold))

      case AskWholeInventory =>
        (this, AnnounceWholeInventory(playerStates(playerId).inventory))

      case AskCommodityInventory(commodity) =>
        (this, AnnounceCommodityInventory(commodity, playerStates(playerId).inventory.commodities(commodity)))

      case AskIfAbleToBuyCommodity(quantity, commodity) =>
        if (playerStates(playerId).gold >= marketState.prices(commodity) * quantity)
          (this, AnnounceCanAfford(commodity, quantity))
        else
          (this, AnnounceCannotAfford(commodity, quantity))

      case AskMarketPriceCommodity(commodity) =>
        (this, AnnounceCommodityMarketPrice(commodity, marketState.prices(commodity)))

      case AskMaxCommodityCanBuy(commodity) =>
        val quantity = Quantity(playerStates(playerId).gold.amount / marketState.prices(commodity).amount)
        (this, AnnounceMaxCommodityCanAfford(commodity, quantity))

      case AskGoldForSellingCommodity(quantity, commodity) =>
        val gold = marketState.prices(commodity) * quantity
        (this, AnnounceGoldFromSellingCommodity(gold, commodity, quantity))

      case AskGoldForSellingMaxCommodity(commodity) =>
        val quantity = playerStates(playerId).inventory.commodities(commodity)
        val gold = marketState.prices(commodity) * quantity
        (this, AnnounceGoldFromSellingMaxCommodity(gold, commodity, quantity))

      case SellCommodity(quantity, commodity) =>
        playerStates(playerId).sell(commodity, quantity, marketState) match {
          case Right(playerState) =>
            val newState = copy(playerStates = playerStates.updated(playerId, playerState))
            (newState, AnnounceSuccessfulSell(commodity, newState.playerStates(playerId).inventory.commodities(commodity)))
          case Left(error) =>
            (this, error.notification)
        }

      case BuyCommodity(quantity, commodity) =>
        playerStates(playerId).buy(commodity, quantity, marketState) match {
          case Right(playerState) =>
            val newState = copy(playerStates = playerStates.updated(playerId, playerState))
            (newState, AnnounceSuccessfulBuy(commodity, newState.playerStates(playerId).inventory.commodities(commodity)))
          case Left(error) =>
            (this, error.notification)
        }
    }
}

object GameState {
  def initial = GameState(Map(), MarketState.initial)
}
