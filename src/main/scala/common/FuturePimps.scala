package common

import scala.concurrent.{ExecutionContext, Future}

trait FuturePimps {

  def serialiseFutures[A, B](l: Iterable[A])(fn: A ⇒ Future[B])
                            (implicit ec: ExecutionContext): Future[List[B]] =
    l.foldLeft(Future(List.empty[B])) {
      (previousFuture, next) ⇒
        for {
          previousResults ← previousFuture
          next ← fn(next)
        } yield previousResults :+ next
    }

}