package nlp

sealed trait NamedEntity
case object Person extends NamedEntity
case object Location extends NamedEntity
case object Organization extends NamedEntity
case object Misc extends NamedEntity
case class Money(amount: Double) extends NamedEntity
case class Number(number: Double) extends NamedEntity
case object Ordinal extends NamedEntity
case class Percent(decimal: Double) extends NamedEntity
case class Date(normalized: String) extends NamedEntity
case class Time(normalized: String) extends NamedEntity
case class Duration(normalized: String) extends NamedEntity
case object Set extends NamedEntity
case object UnknownNamedEntityType extends NamedEntity

object NamedEntityType {
  def fromLabel(label: String, normalized: Option[String]): NamedEntity = (label, normalized) match {
    case ("PERSON", _) => Person
    case ("LOCATION", _) => Location
    case ("ORGANIZATION", _) => Organization
    case ("MISC", _) => Misc
    case ("ORDINAL", _) => Ordinal
    case ("MONEY", Some(moneyString)) => Money(moneyString.substring(1).toDouble)
    case ("NUMBER", Some(numberString)) => Number(numberString.toDouble)
    case ("PERCENT", Some(percentString)) => Percent(percentString.toDouble / 100)
    case ("DATE", Some(normalizedString)) => Date(normalizedString)
    case ("TIME", Some(normalizedString)) => Time(normalizedString)
    case ("DURATION", Some(normalizedString)) => Duration(normalizedString)
    case ("SET", _) => Set
  }
}
