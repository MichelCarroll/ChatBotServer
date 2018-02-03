package nlp


sealed trait PennTag

case object Root extends PennTag
case object NounPhrase extends PennTag
case object VerbPhrase extends PennTag
case object SimpleDeclarativeClause extends PennTag
case object PrepositionalPhrase extends PennTag
case object AdjectivePhrase extends PennTag
case object AdverbPhrase extends PennTag
case object Preposition extends PennTag
case object Determiner extends PennTag
case object Symbol extends PennTag
case object CoordinatingConjunction extends PennTag
case object CardinalNumber extends PennTag
case object ExistantialThere extends PennTag
case object ForeignWord extends PennTag
case object Adjective extends PennTag
case object ComparativeAdjective extends PennTag
case object SuperlativeAdjective extends PennTag
case object ListItemMarker extends PennTag
case object Modal extends PennTag
case object SingularNoun extends PennTag
case object PluralNoun extends PennTag
case object ProperSingularNoun extends PennTag
case object ProperPluralNoun extends PennTag
case object Predeterminer extends PennTag
case object PossessiveEnding extends PennTag
case object PersonalPronoun extends PennTag
case object PossessivePronoun extends PennTag
case object Adverb extends PennTag
case object ComparativeAdverb extends PennTag
case object SuperlativeAdverb extends PennTag
case object InfinitivalTo extends PennTag
case object Interjection extends PennTag
case object BaseFormVerb extends PennTag
case object PastTenseVerb extends PennTag
case object GerundVerb extends PennTag
case object PastParticipleVerb extends PennTag
case object PresentNonThirdPersonVerb extends PennTag
case object PresentThirdPersonVerb extends PennTag
case object WhDeterminer extends PennTag
case object WhPronoun extends PennTag
case object PossessiveWhPronoun extends PennTag
case object WhAdverb extends PennTag
case object SubordinateClause extends PennTag
case object DirectQuestionIntroByWhElement extends PennTag
case object DeclarativeSentenceWithSubjectAuxInv extends PennTag
case object YesNoQuestion extends PennTag
case object WhAdverbPhrase extends PennTag
case object WhNounPhrase extends PennTag
case object WhPrepositionalPhrase extends PennTag

case class UnknownPennTag(label: String) extends PennTag

object PennTag {
  def fromString(string: String): PennTag = string match {
    case "ROOT" => Root
    case "VBD" => PastTenseVerb
    case "VBG" => GerundVerb
    case "VBN" => PastParticipleVerb
    case "VBP" => PresentNonThirdPersonVerb
    case "VBZ" => PresentThirdPersonVerb
    case "WDT" => WhDeterminer
    case "WP" => WhPronoun
    case "WP$" => PossessiveWhPronoun
    case "WRB" => WhAdverb
    case "SBAR" => SubordinateClause
    case "SBARQ" => DirectQuestionIntroByWhElement
    case "SINV" => DeclarativeSentenceWithSubjectAuxInv
    case "SQ" => YesNoQuestion
    case "VP" => VerbPhrase
    case "WHADVP" => WhAdverbPhrase
    case "WHNP" => WhNounPhrase
    case "WHPP" => WhPrepositionalPhrase
    case "CC" => CoordinatingConjunction
    case "CD" => CardinalNumber
    case "DT" => Determiner
    case "EX" => ExistantialThere
    case "FW" => ForeignWord
    case "IN" => Preposition
    case "JJ" => Adjective
    case "JJR" => ComparativeAdjective
    case "JJS" => SuperlativeAdjective
    case "LS" => ListItemMarker
    case "MD" => Modal
    case "NN" => SingularNoun
    case "NNS" => PluralNoun
    case "NNP" => ProperSingularNoun
    case "NNPS" => ProperPluralNoun
    case "PDT" => Predeterminer
    case "POS" => PossessiveEnding
    case "PRP" => PersonalPronoun
    case "PP$" => PossessivePronoun
    case "RB" => Adverb
    case "RBR" => ComparativeAdverb
    case "RBS" => SuperlativeAdverb
    case "SYM" => Symbol
    case "TO" => InfinitivalTo
    case "UH" => Interjection
    case "VB" => BaseFormVerb
    case "ADJP" => AdjectivePhrase
    case "ADVP" => AdverbPhrase
    case "NP" => NounPhrase
    case "PP" => PrepositionalPhrase
    case "S" => SimpleDeclarativeClause
    case other => UnknownPennTag(other)
  }
}

case class Entity(raw: String, lemma: String, namedEntity: Option[NamedEntity])

sealed trait ParseTree {
  val size: Int
  def flatten: List[Entity]
  def tags: List[PennTag]
}
case class Leaf(tag: PennTag, entity: Entity) extends ParseTree {
  val size = 1
  def flatten: List[Entity] = List(entity)
  def tags: List[PennTag] = List(tag)
}
case class Branch(tag: PennTag, children: List[ParseTree]) extends ParseTree {
  lazy val size = children.map(_.size).sum
  def flatten: List[Entity] = children.flatMap(_.flatten)
  def tags: List[PennTag] = tag :: children.flatMap(_.tags)
}