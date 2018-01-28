package facebook.protocol

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json.{DefaultJsonProtocol, JsArray, JsObject, JsString, JsValue, RootJsonReader, RootJsonWriter}


case class MessageResponse(recipient: PageScopedUserId, message: TextMessage)

case class MessageReceived(sender: PageScopedUserId, message: TextMessage)
case class MessagesReceived(entries: Seq[MessageReceived])

case class PageScopedUserId(value: String) extends AnyVal
case class TextMessage(value: String) extends AnyVal

trait FacebookJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit object MessageResponseJsonFormat extends RootJsonWriter[MessageResponse] {
    def write(c: MessageResponse) = JsObject(
      "message_type" -> JsString("RESPONSE"),
      "recipient" -> JsObject(
        "id" -> JsString(c.recipient.value)
      ),
      "message" -> JsObject(
        "text" -> JsString(c.message.value)
      )
    )
  }

  implicit object MessageReceivedJsonFormat extends RootJsonReader[MessagesReceived]  {
    def read(o: JsValue) = {
      val entries = o.asJsObject.fields("entry").asInstanceOf[JsArray].elements
        .flatMap(_.asJsObject.fields("messaging").asInstanceOf[JsArray].elements)
        .map { m =>
          val senderId = m.asJsObject.fields("sender").asJsObject.fields("id").asInstanceOf[JsString].value
          val message = m.asJsObject.fields("message").asJsObject.fields("text").asInstanceOf[JsString].value
          MessageReceived(PageScopedUserId(senderId), TextMessage(message))
        }
      MessagesReceived(entries)
    }
  }
}
