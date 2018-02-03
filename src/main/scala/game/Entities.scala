package game

case class Quantity(amount: Int) {
  def +(other: Quantity) = Quantity(amount + other.amount)
  def -(other: Quantity) = Quantity(amount - other.amount)
  def >=(other: Quantity): Boolean = amount >= other.amount
  def <=(other: Quantity): Boolean = amount <= other.amount
}

case class Gold(amount: Int) {
  def +(other: Gold) = Gold(amount + other.amount)
  def -(other: Gold) = Gold(amount + other.amount)
  def >=(other: Gold): Boolean = amount >= other.amount
  def <=(other: Gold): Boolean = amount <= other.amount
  def *(quantity: Quantity) = Gold(amount * quantity.amount)
}