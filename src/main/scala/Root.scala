import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import chat.ReplyBuilder
import facebook.{AppContext, ChatRoute}
import game.{GameState, PlayerId, UserIntentExtractor}

import scala.io.StdIn

class GameReplyBuilder extends ReplyBuilder {

  var gameState = GameState.initial

  val help = "Give me commands such as: \n- buy 10 lemons \n- sell two lemons \n- how much are lemons worth? \n- how many lemons do I have?"

  def reply(recipientId: String, received: String): List[String] = {
    val playerId = PlayerId(recipientId)
    if(gameState.playerStates.get(playerId).isEmpty) {
      gameState = gameState.join(playerId)
      List(
        "Welcome to TradeBot!",
        "This is a simple game in which you participate in an imaginary commodity market. Your goal is to make as large a profit as possible.",
        s"You start off with ${gameState.playerStates(playerId).gold.amount} gold, and able to buy and sell either lemons or watermelons.",
        help
      )
    }
    else
      UserIntentExtractor.userIntent(received) match {
        case Some(userIntent) =>
          gameState.execute(playerId, userIntent) match {
            case (newGameState, notification) =>
              gameState = newGameState
              List(notification.text)
          }
        case None =>
          List("Sorry, I didn't understand", help)
      }
  }
}

object Root extends App {

  val portNumber = args(0).toInt

  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  implicit val appContext = AppContext(
    "6dac5c2e-bb5c-4b1b-b2c4-113907d51d3e",
    "EAAWoxZCnbZCwkBAFyP2WeYDz9mMZBZB1JHPZBn0kIu7SCvGoyTRQoSTNzLdLeVsqhTpjGb2XBf5vg0wZBui6Pq8TyPZAVNIedSnvLE9rM6efLHnxJ110cX5hwif1HRVEwbnoTRz0tVPfX63T3bNecnJSVifgrXQAl48oRNH9OxZAZBAZDZD"
  )

  val replyBuilder = new GameReplyBuilder()
  val bindingFuture = Http().bindAndHandle(new ChatRoute(replyBuilder).route, "0.0.0.0", portNumber)

  println(s"Server online at http://0.0.0.0:${portNumber}/\nPress RETURN to stop...")
  UserIntentExtractor.warmup()

  StdIn.readLine()
  bindingFuture
    .flatMap(_.unbind())
    .onComplete(_ => system.terminate())


}
