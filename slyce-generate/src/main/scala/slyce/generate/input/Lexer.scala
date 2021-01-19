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
        yields: Yields[String],
    )

  }

}
