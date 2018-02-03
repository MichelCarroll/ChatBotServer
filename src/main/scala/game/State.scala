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

case class PlayerId(id: String) extends AnyVal
case class PlayerState(gold: Gold, inventory: Map[Commodity, Quantity]) {

  def buy(commodity: Commodity, quantity: Quantity, marketState: MarketState): Either[InvalidCommand, PlayerState] = {
    val cost = marketState.prices(commodity) * quantity
    println(cost)
    if(gold >= cost)
      Right(
        copy(
          gold = gold - cost,
          inventory = inventory.updated(commodity, inventory(commodity) + quantity)
        )
      )
    else
      Left(InsufficientMoney)
  }

  def sell(commodity: Commodity, quantity: Quantity, marketState: MarketState): Either[InvalidCommand, PlayerState] = {
    if(inventory(commodity) >= quantity)
      Right(
        copy(
          gold = gold + marketState.prices(commodity) * quantity,
          inventory = inventory.updated(commodity, inventory(commodity) - quantity)
        )
      )
    else
      Left(InsufficientInventory)
  }

}

object PlayerState {
  def initial = PlayerState(
    Gold(100),
    Commodity.all.map((c: Commodity) => c -> Quantity(0)).toMap
  )
}

case class Notification(text: String)

case class GameState(
                    playerStates: Map[PlayerId, PlayerState],
                    marketState: MarketState
                    ) {

  implicit class PlayerStatePimp(playerState: PlayerState) {
    def vulgarizedCommodity(commodity: Commodity): String = playerState.inventory(commodity) match {
      case Quantity(0) => s"no ${commodity.plural}"
      case Quantity(1) => s"1 ${commodity.singular}"
      case Quantity(q) => s"$q ${commodity.plural}"
    }

    def vulgarizedInventory: List[(Commodity, String)] = playerState.inventory
      .toList
      .sortBy(_._2.amount)
      .reverse
      .map { case (c, _) => (c, vulgarizedCommodity(c)) }
  }

  implicit class InvalidCommandPimp(invalidCommand: InvalidCommand) {
    def vulgarized: String = invalidCommand match {
      case InsufficientInventory => "You don't have enough in your inventory!"
      case InsufficientMoney => "You don't have enough gold!"
    }
  }

  def join(playerId: PlayerId): GameState =
    copy(playerStates.updated(playerId, PlayerState.initial))

  def execute(playerId: PlayerId, userIntent: UserIntent): (GameState, Notification) =
    userIntent match {
      case AskWallet =>
        (this, Notification(s"You have ${playerStates(playerId).gold.amount} gold."))

      case AskWholeInventory =>
        val inventoryReport = playerStates(playerId).vulgarizedInventory.map(_._2).mkString(", ")
        (this, Notification(s"You have $inventoryReport."))

      case AskCommodityInventory(commodity) =>
        val commodityReport = playerStates(playerId).vulgarizedCommodity(commodity)
        (this, Notification(s"You have $commodityReport."))

      case AskIfAbleToBuyCommodity(quantity, commodity) =>
        (this, Notification("Not implemented yet"))

      case AskMarketPriceCommodity(commodity) =>
        (this, Notification(s"The ${commodity.plural} are worth ${marketState.prices(commodity).amount} gold."))

      case AskMaxCommodityCanBuy(commodity) =>
        (this, Notification("Not implemented yet"))

      case AskGoldForSellingCommodity(quantity, commodity) =>
        (this, Notification("Not implemented yet"))

      case AskGoldForSellingMaxCommodity(commodity) =>
        (this, Notification("Not implemented yet"))

      case SellCommodity(quantity, commodity) =>
        playerStates(playerId).sell(commodity, quantity, marketState) match {
          case Right(playerState) =>
            (copy(playerStates = playerStates.updated(playerId, playerState)), Notification("OK!"))
          case Left(error) =>
            (this, Notification(error.vulgarized))
        }

      case BuyCommodity(quantity, commodity) =>
        playerStates(playerId).buy(commodity, quantity, marketState) match {
          case Right(playerState) =>
            (copy(playerStates = playerStates.updated(playerId, playerState)), Notification("OK!"))
          case Left(error) =>
            (this, Notification(error.vulgarized))
        }
    }
}

object GameState {
  def initial = GameState(Map(), MarketState.initial)
}
