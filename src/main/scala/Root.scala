import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import facebook.AppContext
import game.UserIntentExtractor
import nlp.{FlatAnnotator, CoreNLP, NER, TreeAnnotator}

import scala.io.StdIn
import sext._
import wordnet.WordNet

class GameReplyBuilder extends ReplyBuilder {
  def reply(received: String): String = {
    UserIntentExtractor.userIntent(received) match {
      case Some(userIntent) => s"You want to $userIntent"
      case None => "Uh?"
    }
  }
}

object Root extends App {

  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  implicit val appContext = AppContext(
    "6dac5c2e-bb5c-4b1b-b2c4-113907d51d3e",
    "EAAWoxZCnbZCwkBAFyP2WeYDz9mMZBZB1JHPZBn0kIu7SCvGoyTRQoSTNzLdLeVsqhTpjGb2XBf5vg0wZBui6Pq8TyPZAVNIedSnvLE9rM6efLHnxJ110cX5hwif1HRVEwbnoTRz0tVPfX63T3bNecnJSVifgrXQAl48oRNH9OxZAZBAZDZD"
  )

  val replyBuilder = new GameReplyBuilder()
  val bindingFuture = Http().bindAndHandle(new ChatRoute(replyBuilder).route, "localhost", 8888)

  println(s"Server online at http://localhost:8888/\nPress RETURN to stop...")
  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())

}
