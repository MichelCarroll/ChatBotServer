package game

import nlp._
import wordnet.WordNet

object UserIntentExtractor {

  implicit class ParseTreePimper(parseTree: ParseTree) {

    private val commodityMap: Map[String, Commodity] =
      Commodity.all.map(c => c.singular -> c).toMap

    private lazy val entities = parseTree.flatten
    private lazy val tags = parseTree.tags
    private lazy val leaves = parseTree.leaves

    def hasLemma(synonymSet: Set[String]): Boolean =
      entities.map(_.lemma).exists(synonymSet.contains)

    def quantity: Option[Int] =
      entities.flatMap(_.namedEntity).collect { case Number(x) => x.toInt}.headOption

    def commodityAndPlurality: Option[(Commodity, Boolean)] =
      leaves.flatMap(leaf =>
        commodityMap
          .get(leaf.entity.lemma)
          .map(c => (c, leaf.tag == PluralNoun))
      ).headOption

    def commodity: Option[Commodity] = commodityAndPlurality.map(_._1)

    def hasQuestionForm: Boolean =
      tags.contains(DeclarativeSentenceWithSubjectAuxInv) ||
        tags.contains(YesNoQuestion) ||
        tags.contains(DirectQuestionIntroByWhElement)

  }

  lazy val wordNet = WordNet()
  lazy val coreNlp = new CoreNLP()
  lazy val annotator = new FlatAnnotator(coreNlp)
  lazy val treeParser = new TreeAnnotator(coreNlp)

  def warmup(): Unit = {
    wordNet
    coreNlp
    annotator
    treeParser
  }

  def userIntent(text: String): Option[UserIntent] =
    treeParser.build(text).headOption.flatMap { parseTree =>

//      println(parseTree.flatten.map(_.raw).mkString(" "))
//      println(parseTree.flatten.map(_.lemma).mkString(" "))
//      println(parseTree.flatten.map(_.lemma).map(x => x -> wordNet.synsets(x)))
//      println(parseTree.tags)

      val can = Set("can", "could")
      val price = Set("price", "worth", "cost", "value", "market")
      val inventory = Set("storage", "inventory", "stock", "stuff", "commodity")
      val buying = Set("buy", "purchase")
      val affording = Set("afford")
      val selling = Set("sell")
      val money = Set("currency", "money", "wallet", "gold", "rich", "wealth", "wealthy")
      val having = Set("have")
      val amount = Set("many", "much", "amount")
      val max = Set("max", "maximum", "most")

      def mentions(synonymSet: Set[String]) = parseTree.hasLemma(synonymSet)
      def omits(synonymSet: Set[String]) = !parseTree.hasLemma(synonymSet)

      parseTree.commodityAndPlurality match {
        case Some((commodity, commodityIsPlural)) =>

          if (parseTree.hasQuestionForm && mentions(money) && mentions(selling))
            if (parseTree.quantity.isEmpty && commodityIsPlural)
              Some(AskGoldForSellingMaxCommodity(commodity))
            else
              Some(AskGoldForSellingCommodity(Quantity(parseTree.quantity.getOrElse(1)), commodity))

          else if (mentions(max) && mentions(money) && mentions(selling))
            Some(AskGoldForSellingMaxCommodity(commodity))

          else if ((mentions(amount) || mentions(max)) && (mentions(buying) || mentions(affording) || mentions(money)))
            Some(AskMaxCommodityCanBuy(commodity))

          else if (mentions(buying) && omits(selling))
            if(parseTree.hasQuestionForm)
              Some(AskIfAbleToBuyCommodity(Quantity(parseTree.quantity.getOrElse(1)), commodity))
            else
              Some(BuyCommodity(Quantity(parseTree.quantity.getOrElse(1)), commodity))

          else if (parseTree.hasQuestionForm && mentions(affording))
            Some(AskIfAbleToBuyCommodity(Quantity(parseTree.quantity.getOrElse(1)), commodity))

          else if (mentions(selling) && omits(buying))
            Some(SellCommodity(Quantity(parseTree.quantity.getOrElse(1)), commodity))

          else if (mentions(amount) && mentions(having))
            Some(AskCommodityInventory(commodity))

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