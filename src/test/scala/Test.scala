
import nlp.ParseTreeBuilder
import org.scalatest._
import sext._

class Test extends FlatSpec with Matchers {

//  "A Stack" should "pop values in last-in-first-out order" in {
//    val stack = new Stack[Int]
//    stack.push(1)
//    stack.push(2)
//    stack.pop() should be (2)
//    stack.pop() should be (1)
//  }
//
//  it should "throw NoSuchElementException if an empty stack is popped" in {
//    val emptyStack = new Stack[Int]
//    a [NoSuchElementException] should be thrownBy {
//      emptyStack.pop()
//    }
//  }

  val parseTreeBuilder = new ParseTreeBuilder()

  "something" should "something" in {
    val tree = parseTreeBuilder.build("Joe is eating an apple.")
    println(tree.treeString)
  }
}