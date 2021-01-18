package slyce.core

import klib.Implicits.BooleanOps

trait Token {

  def span: Token.Span

}

object Token {

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
