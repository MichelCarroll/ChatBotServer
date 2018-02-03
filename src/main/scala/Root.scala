import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import chat.ReplyBuilder
import facebook.{AppContext, ChatRoute}
import game.{GameState, PlayerId, UserIntentExtractor}

import scala.io.StdIn

class GameReplyBuilder extends ReplyBuilder {

  var gameState = GameState.initial

  def reply(recipientId: String, received: String): String = {
    val playerId = PlayerId(recipientId)
    if(gameState.playerStates.get(playerId).isEmpty) {
      gameState = gameState.join(playerId)
      "Welcome to the CommodityBot"
    }
    else
      UserIntentExtractor.userIntent(received) match {
        case Some(userIntent) =>
          gameState.execute(playerId, userIntent) match {
            case (newGameState, notification) =>
              gameState = newGameState
              notification.text
          }
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
  UserIntentExtractor.warmup()

  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())


}
