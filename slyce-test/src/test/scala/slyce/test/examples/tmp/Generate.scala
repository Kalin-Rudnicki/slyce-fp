package slyce.test.examples.tmp

import klib.Implicits._
import slyce.generate._
import input._
import klib.fp.types._
import klib.utils._
import slyce.core._
import slyce.generate.Yields.ToMode
import slyce.test.examples._

object Generate {
  import Lexer.Mode
  import Mode.Line
  import Yields.Yield._
  import Regex._, CharClass._

  val executable: Executable =
    debugExecutable(
      name = "tmp",
      lexer = Lexer(
        startMode = Marked("General"),
        modes = List(
          Mode(
            name = Marked("General"),
            lines = List(
              Line(
                priority = 1,
                regex = Marked(Sequence("tmp")),
                yields = Yields(
                  yields = List(
                    Marked(
                      Yields.Yield.Terminal("tmp"),
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
      grammar = Grammar(
        startNt = Marked("Tmp"),
        nts = List(
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("Tmp")),
            nt = Grammar.StandardNonTerminal.`:`(
              reductions = NonEmptyList.nel(
                List(
                  Marked(
                    Grammar.ListNonTerminal(
                      `type` = Grammar.ListNonTerminal.Type.*,
                      start = IgnoredList(
                        before = Nil,
                        unIgnored = Marked(
                          Grammar.ListNonTerminal(
                            `type` = Grammar.ListNonTerminal.Type.+,
                            start = IgnoredList(
                              before = Nil,
                              unIgnored = Grammar.Identifier.unsafeTerminal(Marked("tmp")),
                              after = Nil,
                            ),
                            repeat = None,
                          ),
                        ),
                        after = Nil,
                      ),
                      repeat = None,
                    ),
                  ),
                ),
              ),
            ),
          ),
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("Tmp2")),
            nt = Grammar.StandardNonTerminal.`:`(
              reductions = NonEmptyList.nel(
                List(
                  Marked(
                    Grammar.ListNonTerminal(
                      `type` = Grammar.ListNonTerminal.Type.*,
                      start = IgnoredList(
                        before = Nil,
                        unIgnored = Marked(
                          Grammar.ListNonTerminal(
                            `type` = Grammar.ListNonTerminal.Type.+,
                            start = IgnoredList(
                              before = Nil,
                              unIgnored = Grammar.Identifier.unsafeTerminal(Marked("tmp")),
                              after = Nil,
                            ),
                            repeat = None,
                          ),
                        ),
                        after = Nil,
                      ),
                      repeat = None,
                    ),
                  ),
                ),
              ),
            ),
          ),
        ),
      ),
    )

}
