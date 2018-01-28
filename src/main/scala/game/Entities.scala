package game

import net.didion.jwnl.data.POS

case class WordSense(lemma: String, pos: POS, senseId: Int)

sealed trait Commodity {
  val wordSense: WordSense
}
case object Lemon extends Commodity {
  val wordSense = WordSense("lemon", POS.NOUN, 1)
}
case object Cup extends Commodity {
  val wordSense = WordSense("cup", POS.NOUN, 1)
}

sealed trait QuantityUnit
case object Single extends QuantityUnit
case object Dozen extends QuantityUnit

case class Quantity(amount: Int, quantityUnit: QuantityUnit)
object Quantity {
  def apply(amount: Int): Quantity = Quantity(amount, Single)
}
