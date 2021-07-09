package slyce.test.examples.tmp

import klib.Implicits._
import slyce.generate._
import input._
import klib.fp.types._
import klib.utils._
import slyce.core._
import slyce.test.examples._

object Generate {
  import Lexer.Mode
  import Mode.Line
  import Yields.Yield._
  import Regex._, CharClass._

  val executable: Executable =
    debugGenerate(
      name = "tmp",
      lexer = Lexer(
        startMode = Marked("General"),
        modes = List(
          Mode(
            name = Marked("General"),
            lines = List(
              Line(
                priority = 1,
                regex = Marked(inclusive('a', 'b')),
                yields = Yields(
                  yields = List(
                    Marked(
                      Yields.Yield.Text(),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
      grammar = Grammar(
        startNt = Marked("S"),
        nts = List(
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("S")),
            nt = Grammar.StandardNonTerminal.`:`(
              reductions = NonEmptyList.nel(
                List(
                  Grammar.Identifier.unsafeNonTerminal(Marked("A")),
                  Grammar.Identifier.unsafeNonTerminal(Marked("A")),
                ),
              ),
            ),
          ),
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("A")),
            nt = Grammar.StandardNonTerminal.`:`(
              reductions = NonEmptyList.nel(
                List(
                  Marked(Grammar.Identifier.raw("a")),
                  Grammar.Identifier.unsafeNonTerminal(Marked("A")),
                ),
                List(
                  Marked(Grammar.Identifier.raw("b")),
                ),
              ),
            ),
          ),
        ),
      ),
    )

}
