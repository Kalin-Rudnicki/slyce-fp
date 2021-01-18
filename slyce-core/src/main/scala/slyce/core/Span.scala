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

  def join(spans: Span*): Maybe[Span] =
    spans.toList match {
      case Span(start, end) :: rest =>
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
          start,
          end,
          rest,
        ).some
      case Nil =>
        None
    }

  def join(spans: Maybe[Span]*): Maybe[Span] =
    join(spans.flatMap(_.toOption): _*)

  final case class Pos private (
      absolutePos: Int,
      lineNo: Int,
      posInLine: Int,
  ) {

    def onChar(c: Char): Pos =
      c match {
        case '\n' =>
          Pos(absolutePos + 1, lineNo + 1, Pos.PosInLineStart)
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
