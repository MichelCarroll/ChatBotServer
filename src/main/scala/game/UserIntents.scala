package game

import sext._
import nlp._
import wordnet.WordNet

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


object UserIntentTester {

  implicit class ParseTreePimper(parseTree: ParseTree) {

    private val commodityMap: Map[String, Commodity] = Map(
      "lemon" -> Lemon,
      "watermelon" -> Watermelon
    )

    private lazy val entities = parseTree.flatten
    private lazy val tags = parseTree.tags

    def hasLemma(synonymSet: Set[String]): Boolean =
      entities.map(_.lemma).exists(synonymSet.contains)

    def quantity: Option[Int] =
      entities.flatMap(_.namedEntity).collect { case Number(x) => x.toInt}.headOption

    def commodity: Option[Commodity] =
      entities.map(_.lemma).flatMap(commodityMap.get).headOption

    def hasQuestionForm: Boolean =
      tags.contains(DeclarativeSentenceWithSubjectAuxInv) || tags.contains(DirectQuestionIntroByWhElement)

  }

  lazy val wordNet = WordNet()
  lazy val coreNlp = new CoreNLP()
  lazy val annotator = new FlatAnnotator(coreNlp)
  lazy val treeParser = new TreeAnnotator(coreNlp)

  def testUserIntent(text: String): Option[UserIntent] =
    treeParser.build(text).headOption.flatMap { parseTree =>

      println(parseTree.flatten.map(_.raw).mkString(" "))
      println(parseTree.flatten.map(_.lemma).mkString(" "))
      println(parseTree.flatten.map(_.lemma).map(x => x -> wordNet.synsets(x)).valueTreeString)
      println(parseTree.tags)

      val can = Set("can", "could")
      val price = Set("price", "worth", "cost", "value", "market")
      val inventory = Set("storage", "inventory", "stock", "stuff", "commodity")
      val buying = Set("buy", "purchase")
      val selling = Set("sell")
      val money = Set("currency", "money", "wallet", "gold", "rich", "wealth", "wealthy")
      val having = Set("have")

      def mentions(synonymSet: Set[String]) = parseTree.hasLemma(synonymSet)
      def omits(synonymSet: Set[String]) = !parseTree.hasLemma(synonymSet)

      parseTree.commodity match {
        case Some(commodity) =>

          if (mentions(buying) && omits(selling))
            if(parseTree.hasQuestionForm)
              Some(AskIfAbleToBuyCommodity(Quantity(parseTree.quantity.getOrElse(1)), commodity))
            else
              Some(BuyCommodity(Quantity(parseTree.quantity.getOrElse(1)), commodity))

          else if (mentions(selling) && omits(buying))
            Some(SellCommodity(Quantity(parseTree.quantity.getOrElse(1)), commodity))

          else if (mentions(price))
            Some(AskMarketPriceCommodity(commodity))

          else
            Some(AskCommodityInventory(commodity))

        case None =>

          if (mentions(money))
            Some(AskWallet)

          else if (parseTree.hasQuestionForm && mentions(having))
            Some(AskWholeInventory)

          else if (mentions(inventory))
            Some(AskWholeInventory)

          else
            None

      }
    }

}