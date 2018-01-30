package game

sealed trait Commodity
case object Lemon extends Commodity
case object Watermelon extends Commodity

sealed trait QuantityUnit
case object Single extends QuantityUnit
case object Dozen extends QuantityUnit
case object Crate extends QuantityUnit

case class Quantity(amount: Int, quantityUnit: QuantityUnit)
object Quantity {
  def apply(amount: Int): Quantity = Quantity(amount, Single)
}
