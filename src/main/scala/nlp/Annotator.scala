package nlp

import edu.stanford.nlp.trees.{Tree => JavaTree}
import scala.collection.JavaConverters._

class Annotator {

  val javaObject = new CoreNLP()

  case class ExtraAnnotations(lemmas: List[String], namedEntity: List[Option[String]], normalizedNamedEntity: List[Option[String]])

  def scalaTree(extraAnnotations: ExtraAnnotations)(offset: Int, tree: JavaTree): ParseTree =
    if(tree.isPreTerminal)
      Leaf(
        PennTag.fromString(tree.value()),
        Entity(
          tree.children().head.value(),
          extraAnnotations.lemmas(offset),
          extraAnnotations.namedEntity(offset).map(label =>
            NamedEntityType.fromLabel(label, extraAnnotations.normalizedNamedEntity(offset))
          )
        )
      )
    else
      Branch(
        PennTag.fromString(tree.value()),
        //TODO: Highly inefficient
        tree.children().toList.foldLeft((0, List[ParseTree]())) { case ((i, trees), child) =>
          val tree = scalaTree(extraAnnotations)(offset + i, child)
          (i + tree.size, tree :: trees)
        }._2.reverse
      )

  def build(text: String): List[ParseTree] =
    javaObject.run(text).asScala.toList.map { result =>
      scalaTree (ExtraAnnotations(
        result.lemmas.asScala.toList,
        result.namedEntityTypes.asScala.toList.map {
          case "O" => None
          case x => Some(x)
        },
        result.normalizedNamedEntities.asScala.toList.map {
          case null => None
          case x => Some(x)
        }
      )) (0, result.parseTree)
    }

}
