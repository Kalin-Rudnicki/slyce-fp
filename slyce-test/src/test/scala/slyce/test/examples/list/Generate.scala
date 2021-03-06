package slyce.test.examples.list

import klib.Implicits._
import klib.utils._

import slyce.core._
import slyce.generate._
import slyce.generate.input._
import slyce.generate.main._
import slyce.test.examples._

object Generate {
  import Lexer.Mode
  import Mode.Line
  import Yields.ToMode
  import Yields.Yield._
  import Regex._, CharClass._

  lazy val executable: Executable =
    debugGenerate(
      BuildInput(
        name = "list",
        lexer = Lexer(
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
                      "ToOther",
                    ),
                  ),
                  yields = Yields(
                    yields = List(Marked(Terminal("other"))),
                    toMode = Marked(ToMode.Push("Other")),
                  ),
                ),
                Line(
                  priority = 5,
                  regex = Marked(
                    Sequence(
                      `[A-Z]` | `[a-z]`,
                      `[A-Za-z_\\d]`.repeat(4, 9.some),
                    ),
                  ),
                  yields = Yields(
                    yields = List(Marked(Terminal("var"))),
                  ),
                ),
              ),
            ),
            Mode(
              name = Marked("Other"),
              lines = List(
                Line(
                  priority = 6,
                  regex = Marked(
                    inclusive(' ', '\t', '\n'),
                  ),
                  yields = Yields(
                    yields = List(),
                  ),
                ),
                Line(
                  priority = 7,
                  regex = Marked(
                    Sequence("Back"),
                  ),
                  yields = Yields(
                    yields = List(),
                    toMode = Marked(ToMode.Pop),
                  ),
                ),
              ),
            ),
          ),
        ),
        grammar = Grammar(
          startNt = ???, // TODO (KR) :
          nts = ???, // TODO (KR) :
        ),
      ),
    )

}
