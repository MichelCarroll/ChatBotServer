
import nlp.Annotator
import org.scalatest._
import sext._
import wordnet.WordNet

class Test extends FlatSpec with Matchers {

  val parseTreeBuilder = new Annotator()

  "something" should "something" in {
    val tree = parseTreeBuilder.build("I will be arriving at 12pm with $12.05 in my wallet.")
    println(tree.treeString)
  }
}