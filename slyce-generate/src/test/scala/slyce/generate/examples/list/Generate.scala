package slyce.generate.examples.list

import klib.Implicits._
import klib.fp.types._
import slyce.generate.examples.run
import slyce.generate._, input._
import slyce.core._

object Generate extends App {
  import Lexer.Mode
  import Mode.Line
  import Yields.Yield._
  import Regex._, CharClass._

  run(
    Lexer(
      startMode = Marked("General"),
      modes = List(
        Mode(
          name = Marked("General"),
          lines = List(
            Line(
              priority = 1,
              regex = Marked(
                inclusive('(', ')', ','),
              ),
              yields = Yields(
                yields = List(Marked(Text())),
              ),
            ),
            Line(
              priority = 2,
              regex = Marked(
                inclusive(' ', '\t', '\n'),
              ),
              yields = Yields(
                yields = List(),
              ),
            ),
            Line(
              priority = 3,
              regex = Marked(
                `\\d`.atLeastOnce,
              ),
              yields = Yields(
                yields = List(Marked(Terminal("int"))),
              ),
            ),
            Line(
              priority = 4,
              regex = Marked(
                Sequence(
                  `[A-Z]` | `[a-z]`,
                  `[A-Za-z_\\d]`.repeat(4, 9.some),
                ),
              ),
              yields = Yields(
                yields = List(Marked(Terminal("int"))),
              ),
            ),
          ),
        ),
      ),
    ),
  )

}
