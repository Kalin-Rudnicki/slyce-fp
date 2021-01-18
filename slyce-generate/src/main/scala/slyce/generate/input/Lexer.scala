package slyce.generate.input

import klib.fp.types._
import slyce.core._
import slyce.generate._

final case class Lexer(
    startMode: Marked[String],
    modes: List[Lexer.Mode],
)

object Lexer {

  final case class Mode(
      name: Marked[String],
      lines: List[Mode.Line],
  )

  object Mode {

    final case class Line(
        priority: Int,
        regex: Marked[Regex],
        yields: Line.Yields,
    )

    object Line {

      final case class Yields(
          yields: List[Marked[Yields.Yield]],
          toMode: Marked[Yields.ToMode] = Marked(Yields.ToMode.Same),
      )

      object Yields {

        sealed trait Yield {

          val subString: (Maybe[Int], Maybe[Int])

        }
        object Yield {

          final case class Text(
              subString: (Maybe[Int], Maybe[Int]) = (None, None),
          ) extends Yield

          final case class Terminal(
              name: String,
              subString: (Maybe[Int], Maybe[Int]) = (None, None),
          ) extends Yield

        }

        sealed trait ToMode
        object ToMode {

          case object Same extends ToMode

          final case class To(modeName: String) extends ToMode

          final case class Push(modeName: String) extends ToMode

          case object Pop extends ToMode

        }

      }

    }

  }

}
