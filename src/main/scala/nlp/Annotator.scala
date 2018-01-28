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
          extraAnnotations.lemmas(offset),
          tree.children().head.value(),
          extraAnnotations.namedEntity(offset).map(label =>
            NamedEntityType.fromLabel(label, extraAnnotations.normalizedNamedEntity(offset))
          )
        )
      )
    else
      Branch(
        PennTag.fromString(tree.value()),
        tree.children().toList.zipWithIndex.map { case (child, i) =>
          scalaTree(extraAnnotations)(offset + i, child)
        }
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
