package game

sealed trait Commodity
case object Lemon extends Commodity
case object Watermelon extends Commodity

case class Quantity(amount: Int)
case class Gold(amount: Int)