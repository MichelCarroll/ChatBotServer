package game

sealed trait UserIntent

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