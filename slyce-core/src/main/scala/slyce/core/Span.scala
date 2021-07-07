package slyce.core

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.types._

final case class Span(
    start: Span.Pos,
    end: Span.Pos,
) {

  def toString(showAbsolute: Boolean): String =
    s"Span(${start.toString(showAbsolute)} -> ${end.toString(showAbsolute)})"

  override def toString: String =
    toString(false)

}

object Span {

  def joinNE(span0: Span, spanN: Span*): Span = {
    import Ordering.Implicits._

    @tailrec
    def loop(
        start: Span.Pos,
        end: Span.Pos,
        queue: List[Span],
    ): Span =
      queue match {
        case Span(_start, _end) :: rest =>
          loop(
            start.min(_start),
            end.min(_end),
            rest,
          )
        case Nil =>
          Span(start, end)
      }

    loop(
      span0.start,
      span0.end,
      spanN.toList,
    )
  }

  def joinId(spans: Span*): Maybe[Span] =
    spans.toList match {
      case head :: tail =>
        joinNE(head, tail: _*).some
      case Nil =>
        None
    }

  def joinM(spans: Maybe[Span]*): Maybe[Span] =
    joinId(spans.flatMap(_.toOption): _*)

  final case class Pos private (
      absolutePos: Int,
      lineNo: Int,
      posInLine: Int,
  ) {

    def onChar(c: Char): Pos =
      c match {
        case '\n' => Pos(absolutePos + 1, lineNo + 1, Pos.PosInLineStart)
        case _    => Pos(absolutePos + 1, lineNo, posInLine + 1)
      }

    def toString(showAbsolute: Boolean): String =
      s"${showAbsolute ? s"$absolutePos @ " | ""}$lineNo:$posInLine"

    override def toString: String =
      toString(false)

  }

  object Pos {

    private val AbsolutePosStart = 1
    private val LineNoStart = 1
    private val PosInLineStart = 1

    val Start: Pos = Pos(AbsolutePosStart, LineNoStart, PosInLineStart)

    implicit val posOrdering: Ordering[Pos] = Ordering.by(_.absolutePos)

  }

}
