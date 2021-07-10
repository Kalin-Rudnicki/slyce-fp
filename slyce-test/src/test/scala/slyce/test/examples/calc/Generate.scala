package slyce.test.examples.calc

import klib.fp.types._
import klib.utils._

import slyce.generate._
import slyce.generate.input._
import slyce.generate.main._

import slyce.core._
import slyce.test.examples._

object Generate {
  import Lexer.Mode
  import Mode.Line
  import Regex._
  import CharClass._
  import Yields.Yield._

  lazy val executable: Executable =
    debugGenerate(
      BuildInput(
        name = "calc",
        lexer = Lexer(
          startMode = Marked("General"),
          modes = List(
            Mode(
              name = Marked("General"),
              lines = List(
                Line(
                  priority = 1,
                  regex = Marked(
                    inclusive('{', '}', '(', ')', '=', ';'),
                  ),
                  yields = Yields(
                    yields = List(Marked(Text())),
                  ),
                ),
                Line(
                  priority = 2,
                  regex = Marked(
                    Regex.Sequence(
                      CharClass.`[A-Z]` | CharClass.`[a-z]` | inclusive('_'),
                      CharClass.`[A-Za-z_\\d]`.anyAmount,
                    ),
                  ),
                  yields = Yields(
                    yields = List(Marked(Terminal("variable"))),
                  ),
                ),
                Line(
                  priority = 3,
                  regex = Marked(
                    Sequence(
                      inclusive('-').maybe,
                      CharClass.`\\d`.atLeastOnce,
                    ),
                  ),
                  yields = Yields(
                    yields = List(Marked(Terminal("int"))),
                  ),
                ),
                Line(
                  priority = 4,
                  regex = Marked(
                    inclusive('+', '-'),
                  ),
                  yields = Yields(
                    yields = List(Marked(Terminal("addOp"))),
                  ),
                ),
                Line(
                  priority = 5,
                  regex = Marked(
                    inclusive('*', '/'),
                  ),
                  yields = Yields(
                    yields = List(Marked(Terminal("multOp"))),
                  ),
                ),
                Line(
                  priority = 6,
                  regex = Marked(
                    inclusive('^'),
                  ),
                  yields = Yields(
                    yields = List(Marked(Terminal("powOp"))),
                  ),
                ),
                Line(
                  priority = 7,
                  regex = Marked(
                    inclusive(' ', '\t', '\n').atLeastOnce,
                  ),
                  yields = Yields(
                    yields = List(),
                  ),
                ),
              ),
            ),
          ),
        ),
        grammar = Grammar(
          startNt = Marked("Prog"),
          nts = List(
            Grammar.NT(
              name = Grammar.Identifier.unsafeNonTerminal(Marked("Prog")),
              nt = Grammar.StandardNonTerminal.`:`(
                reductions = NonEmptyList.nel(
                  List(
                    Marked(Grammar.Identifier.raw("{")),
                    Marked(Grammar.Identifier("Assign")),
                    Marked(Grammar.Identifier.raw(";")),
                    Marked(Grammar.Identifier.raw("}")),
                  ),
                ),
              ),
            ),
            Grammar.NT(
              name = Grammar.Identifier.unsafeNonTerminal(Marked("Lines")),
              nt = Grammar.ListNonTerminal(
                `type` = Grammar.ListNonTerminal.Type.*,
                start = IgnoredList(
                  before = Nil,
                  unIgnored = Marked(Grammar.Identifier("Line")),
                  after = List(
                    Marked(Grammar.Identifier(";")),
                  ),
                ),
                repeat = None,
              ),
            ),
            Grammar.NT(
              name = Grammar.Identifier.unsafeNonTerminal(Marked("Line")),
              nt = Grammar.StandardNonTerminal.`:`(
                reductions = NonEmptyList.nel(
                  List(
                    Marked(Grammar.Identifier("Assign")),
                  ),
                  List(
                    Marked(Grammar.Identifier("Expr")),
                  ),
                ),
              ),
            ),
            Grammar.NT(
              name = Grammar.Identifier.unsafeNonTerminal(Marked("Assign")),
              nt = Grammar.StandardNonTerminal.`:`(
                reductions = NonEmptyList.nel(
                  List(
                    Marked(Grammar.Identifier("variable")),
                    Marked(Grammar.Identifier("=")),
                    Marked(Grammar.Identifier("Expr")),
                  ),
                ),
              ),
            ),
            Grammar.NT(
              name = Grammar.Identifier.unsafeNonTerminal(Marked("Expr")),
              nt = Grammar.AssocNonTerminal(
                assocs = NonEmptyList.nel(
                  (Marked(Grammar.AssocNonTerminal.Type.Right), Marked(Grammar.Identifier("powOp"))),
                  (Marked(Grammar.AssocNonTerminal.Type.Left), Marked(Grammar.Identifier("multOp"))),
                  (Marked(Grammar.AssocNonTerminal.Type.Left), Marked(Grammar.Identifier("addOp"))),
                ),
                base = Grammar.StandardNonTerminal.^(
                  reductions = NonEmptyList.nel(
                    IgnoredList(
                      before = Nil,
                      unIgnored = Marked(Grammar.Identifier("variable")),
                      after = Nil,
                    ),
                    IgnoredList(
                      before = Nil,
                      unIgnored = Marked(Grammar.Identifier("int")),
                      after = Nil,
                    ),
                    IgnoredList(
                      before = List(Marked(Grammar.Identifier("("))),
                      unIgnored = Marked(Grammar.Identifier("Expr")),
                      after = List(Marked(Grammar.Identifier(")"))),
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
