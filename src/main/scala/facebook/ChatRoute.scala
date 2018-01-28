
import Root.system
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.Materializer
import facebook.AppContext
import facebook.protocol.{FacebookJsonSupport, MessageResponse, MessagesReceived, TextMessage}
import spray.json._

import scala.concurrent.{ExecutionContext, Future}


class ChatRoute(replyBuilder: ReplyBuilder)
               (implicit val system: ActorSystem,
                val materializer: Materializer,
                val appContext: AppContext)
  extends FacebookJsonSupport {

  implicit val executionContext = system.dispatcher

  val route =
    get {
      parameters(
        "hub.mode".as[String],
        "hub.challenge".as[String],
        "hub.verify_token".as[String],
      ) { case (mode, challenge, verifyToken) =>
        if(verifyToken == appContext.verifyToken)
          complete(StatusCodes.OK -> HttpEntity(ContentTypes.`text/html(UTF-8)`, challenge))
        else
          complete(StatusCodes.Unauthorized -> HttpEntity(ContentTypes.`text/html(UTF-8)`, "Invalid Verify Token"))
      }
    } ~
      post {
        entity(as[MessagesReceived]) { messagesReceived =>

          val responses = messagesReceived.entries
            .map(entry => MessageResponse(entry.sender, TextMessage(replyBuilder.reply(entry.message.value))))
            .map(response =>
              Http().singleRequest(HttpRequest(
                method = HttpMethods.POST,
                uri = s"https://graph.facebook.com/v2.6/me/messages?access_token=${appContext.pageAccessToken}"
              ).withEntity(ContentTypes.`application/json`, response.toJson.toString()))
            )

          complete(Future.sequence(responses).map(_ => StatusCodes.OK -> HttpEntity(ContentTypes.`text/html(UTF-8)`, "OK")))
        }
      }


}
