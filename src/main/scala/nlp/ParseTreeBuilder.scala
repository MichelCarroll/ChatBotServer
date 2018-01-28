package nlp

import edu.stanford.nlp.trees.{Tree => JavaTree}
import scala.collection.JavaConverters._

class ParseTreeBuilder {

  val javaObject = new CoreNLP()

  def scalaTree(lemmas: List[String])(offset: Int, tree: JavaTree): ParseTree =
    if(tree.isPreTerminal)
      Leaf(
        PennTag.fromString(tree.value()),
        lemmas(offset),
        tree.children().head.value()
      )
    else
      Branch(
        PennTag.fromString(tree.value()),
        tree.children().toList.zipWithIndex.map { case (child, i) =>
          scalaTree(lemmas)(offset + i, child)
        }
      )

  def build(text: String): List[ParseTree] =
    javaObject.run(text).asScala.toList.map { result =>
      scalaTree(result.lemmas.asScala.toList)(0, result.parseTree)
    }

}
