package game

import nlp._

sealed trait UserIntent
case class BuyCommodity(quantity: Quantity, commodity: Commodity) extends UserIntent
case class AskIfAbleToBuyCommodity(quantity: Quantity, commodity: Commodity) extends UserIntent

object UserIntentTester {

  lazy val coreNlp = new CoreNLP()
  lazy val annotator = new FlatAnnotator(coreNlp)
  lazy val treeParser = new TreeAnnotator(coreNlp)

  def testUserIntent(text: String): Option[UserIntent] =
    treeParser.build(text).headOption.flatMap { parseTree =>

        val flattenedTree = parseTree.flatten

        val buySynonyms = Set("buy", "purchase")
        val commodityMap: Map[String, Commodity] = Map(
          "lemon" -> Lemon,
          "watermelon" -> Watermelon
        )

        val commodityOpt = flattenedTree.map(_.lemma).flatMap(commodityMap.get).headOption
        val hasBuySynonym = flattenedTree.map(_.lemma).exists(buySynonyms.contains)
        val quantityOpt = flattenedTree.flatMap(_.namedEntity).collect { case Number(x) => x.toInt}.headOption

        (commodityOpt, hasBuySynonym, quantityOpt) match {
          case (Some(commodity), true, Some(quantity)) =>
            Some(BuyCommodity(Quantity(quantity), commodity))
          case (Some(commodity), true, None) =>
            Some(BuyCommodity(Quantity(1), commodity))
          case _ =>
            None
//          case Branch(Root,List(
//            Branch(DeclarativeSentenceWithSubjectAuxInv,List(
//              Leaf(Modal,Entity(can,can,None)),
//              Branch(NounPhrase,List(
//                Leaf(PersonalPronoun,Entity(I,I,None))
//              )),
//              Branch(VerbPhrase,List(
//                Leaf(BaseFormVerb,Entity(buy,buy,None)),
//                Branch(NounPhrase,List(
//                  Leaf(CardinalNumber,Entity(two,two,Some(Number(2.0)))),
//                  Leaf(PluralNoun,Entity(lemons,lemon,None))
//                ))
//              ))
//            ))
//          ))
        }
      }

}