package game

sealed trait Commodity {
  def initialValue: Gold
  def singular: String
  def plural: String
}
case object Lemon extends Commodity {
  def initialValue = Gold(4)
  def singular = "lemon"
  def plural = "lemons"
}
case object Watermelon extends Commodity {
  def initialValue = Gold(10)
  def singular = "watermelon"
  def plural = "watermelons"
}

object Commodity {
  def all: Set[Commodity] = Set(Lemon, Watermelon)
}