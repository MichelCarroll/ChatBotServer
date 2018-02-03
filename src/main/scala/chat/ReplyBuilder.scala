package chat

trait ReplyBuilder {
  def reply(recipientId: String, received: String): List[String]
}
