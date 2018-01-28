package game

import net.didion.jwnl.data.POS
import nlp._
import wordnet.WordNet

sealed trait UserIntent
case class BuyCommodity(quantity: Quantity, commodity: Commodity) extends UserIntent


class UserIntentTester(implicit val wordNet: WordNet) {

  private def lemmasWithMeaning(wordSense: WordSense): Set[String] = {
    val x = wordNet.synset(wordSense.lemma, wordSense.pos, wordSense.senseId).getWords
    x.map(_.getLemma).toSet
  }

  private def testForTransitiveVerbWithQuantity(possibleVerbLemmas: Set[String], possibleObjectLemmas: Set[String])(tree: ParseTree): Option[Quantity] = tree match {

    case Branch(_,List(
      Leaf(_,Entity(_, verbLemma,_)),
      Branch(NounPhrase,List(
        Leaf(_, qualifier),
        Leaf(_,Entity(_, objectLemma,_)))
      )
    )) if possibleVerbLemmas.contains(verbLemma) && possibleObjectLemmas.contains(objectLemma) =>
      qualifier match {
        case Entity(_,_,Some(Number(quantity))) =>
          Some(Quantity(quantity.toInt, Single))
        case Entity(_,"a",_) =>
          Some(Quantity(1, Single))
        case _ =>
          None
      }

    case Branch(_,List(
      Leaf(_,Entity(_,verbLemma,None)),
      Branch(NounPhrase,List(
        Branch(NounPhrase,List(
          Leaf(_,qualifier),
          Leaf(_,Entity(_,"dozen",None))
        )),
        Branch(PrepositionalPhrase,List(
          Leaf(Preposition,Entity(_,"of",None)),
          Branch(NounPhrase,List(
            Leaf(_,Entity(_,objectLemma,None))
          ))
        ))
      ))
    )) if possibleVerbLemmas.contains(verbLemma) && possibleObjectLemmas.contains(objectLemma) =>
      qualifier match {
        case Entity(_,_,Some(Number(quantity))) =>
          Some(Quantity(quantity.toInt, Dozen))
        case Entity(_,"a",_) =>
          Some(Quantity(1, Dozen))
        case _ =>
          None
      }

    case Branch(_,List(
      Leaf(_,Entity(_,verbLemma,None)),
      Branch(NounPhrase,List(
        Leaf(_, qualifier),
        Leaf(_,Entity(_,objectLemma,None)),
        Leaf(_,Entity(_, "dozen",None))
      ))
    )) if possibleVerbLemmas.contains(verbLemma) && possibleObjectLemmas.contains(objectLemma) =>
      qualifier match {
        case Entity(_,_,Some(Number(quantity))) =>
          Some(Quantity(quantity.toInt, Dozen))
        case Entity(_,"a",_) =>
          Some(Quantity(1, Dozen))
        case _ =>
          None
      }

    case Branch(_, children) =>
      children.flatMap(testForTransitiveVerbWithQuantity(possibleVerbLemmas, possibleObjectLemmas)).headOption

    case _ =>
      None
  }

  private def testBuyCommodity(commodity: Commodity, tree: ParseTree): Option[UserIntent] =
    testForTransitiveVerbWithQuantity(
      lemmasWithMeaning(WordSense("buy", POS.VERB, 1)),
      lemmasWithMeaning(commodity.wordSense)
    )(tree).map(BuyCommodity(_, commodity))

  def testUserIntent(tree: ParseTree): Option[UserIntent] =
    testBuyCommodity(Lemon, tree)
      .orElse(testBuyCommodity(Cup, tree))

}