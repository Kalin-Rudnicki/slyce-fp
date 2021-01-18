package slyce.core

import klib.Implicits._
import klib.fp.types._

import scala.annotation.tailrec

trait Token {

  def span: Token.Span

}

object Token {

  final case class Span(
      start: Span.Pos,
      end: Span.Pos,
  ) {

    // NOTE : `spans` are assumed to already be in order
    def join(spans: List[Maybe[Span]]): Maybe[Span] =
      spans.flatMap(_.toOption) match {
        case first :: rest =>
          @tailrec
          def loop(
              current: Span,
              remaining: List[Span],
          ): Span =
            remaining match {
              case head :: tail =>
                loop(head, tail)
              case Nil =>
                Span(first.start, current.end)
            }

          loop(first, rest).some
        case Nil =>
          None
      }

    def toString(showAbsolute: Boolean): String =
      s"Span(${start.toString(showAbsolute)} -> ${end.toString(showAbsolute)})"

    override def toString: String =
      toString(false)

  }

  object Span {

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

    }

  }

}
