package slyce.core

import klib.Implicits._
import klib.fp.types._

final case class Source(input: String) {
  val chars: List[Char] = input.toList

  def mark(messages: List[Marked[String]]): String = {
    val (eofs, _marked) =
      messages.partitionMap { msg =>
        msg.span match {
          case Some(span) =>
            scala.Right((msg.value, span))
          case None =>
            scala.Left(msg.value)
        }
      }
    val marked = _marked.sortBy(_._2.start)

    // TODO (KR) :
    ???
  }

}
