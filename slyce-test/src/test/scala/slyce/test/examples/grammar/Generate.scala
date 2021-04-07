package slyce.test.examples.grammar

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import slyce.generate._
import input._
import slyce.core._
import slyce.test.examples._

object Generate {
  import Lexer.Mode
  import Mode.Line
  import Regex._
  import CharClass._
  import Yields.Yield._

  val executable: Executable =
    debugExecutable(
      "grammar",
      lexer = Lexer(
        startMode = Marked("General"),
        modes = List(
          Mode(
            name = Marked("General"),
            lines = List(
              Line(
                priority = 1,
                regex = Marked(Sequence("@start")),
                yields = Yields(
                  yields = List(Marked(Text())),
                ),
              ),
            ),
          ),
        ),
      ),
      grammar = Grammar(
        startNt = Marked("Grammar"),
        nts = List(
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("Grammar")),
            nt = Grammar.StandardNonTerminal.`:`(
              reductions = NonEmptyList.nel(
                List(
                  Marked(Grammar.Identifier.raw("@start")),
                  Marked(Grammar.Identifier("nonTerminal")),
                  Marked(
                    Grammar.ListNonTerminal(
                      `type` = Grammar.ListNonTerminal.Type.*,
                      start = IgnoredList(
                        before = Nil,
                        unIgnored = Marked(Grammar.Identifier("Mode")),
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
            name = Grammar.Identifier.unsafeNonTerminal(Marked("NonTerminal")),
            nt = Grammar.StandardNonTerminal.`:`(
              reductions = NonEmptyList.nel(
                List(
                  Marked(Grammar.Identifier("nonTerminal")),
                  Marked(Grammar.Identifier("NTBody")),
                ),
              ),
            ),
          ),
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("NTBody")),
            nt = Grammar.StandardNonTerminal.^(
              reductions = NonEmptyList.nel(
                IgnoredList(
                  before = Nil,
                  unIgnored = Marked(Grammar.Identifier("StandardNT")),
                  after = Nil,
                ),
                IgnoredList(
                  before = Nil,
                  unIgnored = Marked(Grammar.Identifier("ListNT")),
                  after = Nil,
                ),
                IgnoredList(
                  before = Nil,
                  unIgnored = Marked(Grammar.Identifier("AssocNT")),
                  after = Nil,
                ),
              ),
            ),
          ),
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("ElementList")),
            nt = Grammar.ListNonTerminal(
              `type` = Grammar.ListNonTerminal.Type.*,
              start = IgnoredList(
                before = Nil,
                unIgnored = Grammar.Identifier.unsafeNonTerminal(Marked("Element")),
                after = Nil,
              ),
              repeat = None,
            ),
          ),
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("UnIgnoredElementList")),
            nt = Grammar.StandardNonTerminal.`:`(
              reductions = NonEmptyList.nel(
                List(
                  Marked(Grammar.Identifier("Element")),
                ),
                List(
                  Marked(Grammar.Identifier("ElementList")),
                  Marked(Grammar.Identifier.raw("^")),
                  Marked(Grammar.Identifier("Element")),
                  Marked(Grammar.Identifier("ElementList")),
                ),
              ),
            ),
          ),
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("StandardNT")),
            nt = Grammar.StandardNonTerminal.^(
              reductions = NonEmptyList.nel(
                IgnoredList(
                  before = Nil,
                  unIgnored = Marked(Grammar.Identifier("BasicNT")),
                  after = Nil,
                ),
                IgnoredList(
                  before = Nil,
                  unIgnored = Marked(Grammar.Identifier("LiftNT")),
                  after = Nil,
                ),
              ),
            ),
          ),
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("BasicNT")),
            nt = Grammar.StandardNonTerminal.`:`(
              reductions = NonEmptyList.nel(
                List(
                  Marked(Grammar.Identifier.raw(":")),
                  Marked(
                    Grammar.ListNonTerminal(
                      `type` = Grammar.ListNonTerminal.Type.+,
                      start = IgnoredList(
                        before = Nil,
                        unIgnored = Marked(Grammar.Identifier("ElementList")),
                        after = Nil,
                      ),
                      repeat = IgnoredList(
                        before = List(
                          Marked(Grammar.Identifier.raw("|")),
                        ),
                        unIgnored = Marked(Grammar.Identifier("ElementList")),
                        after = Nil,
                      ).some,
                    ),
                  ),
                ),
              ),
            ),
          ),
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("LiftNT")),
            nt = Grammar.StandardNonTerminal.`:`(
              reductions = NonEmptyList.nel(
                List(
                  Marked(Grammar.Identifier.raw("^")),
                  Marked(
                    Grammar.ListNonTerminal(
                      `type` = Grammar.ListNonTerminal.Type.+,
                      start = IgnoredList(
                        before = Nil,
                        unIgnored = Marked(Grammar.Identifier("UnIgnoredElementList")),
                        after = Nil,
                      ),
                      repeat = IgnoredList(
                        before = List(
                          Marked(Grammar.Identifier.raw("|")),
                        ),
                        unIgnored = Marked(Grammar.Identifier("UnIgnoredElementList")),
                        after = Nil,
                      ).some,
                    ),
                  ),
                ),
              ),
            ),
          ),
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("ListNT")),
            nt = Grammar.StandardNonTerminal.`:`(
              reductions = NonEmptyList.nel(
                List(
                  Marked(Grammar.Identifier("listType")),
                  Marked(Grammar.Identifier("UnIgnoredElementList")),
                ),
                List(
                  Marked(Grammar.Identifier("listType")),
                  Marked(Grammar.Identifier("UnIgnoredElementList")),
                  Marked(Grammar.Identifier.raw(".")),
                  Marked(Grammar.Identifier("UnIgnoredElementList")),
                ),
              ),
            ),
          ),
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("AnonListNT")),
            nt = Grammar.StandardNonTerminal.`:`(
              reductions = NonEmptyList.nel(
                List(
                  Marked(Grammar.Identifier("Element")),
                  Marked(Grammar.Identifier("listType")),
                ),
                List(
                  Marked(Grammar.Identifier.raw("(")),
                  Marked(Grammar.Identifier("UnIgnoredElementList")),
                  Marked(Grammar.Identifier.raw(")")),
                  Marked(Grammar.Identifier("listType")),
                ),
                List(
                  Marked(Grammar.Identifier.raw("(")),
                  Marked(Grammar.Identifier("UnIgnoredElementList")),
                  Marked(Grammar.Identifier.raw(".")),
                  Marked(Grammar.Identifier("UnIgnoredElementList")),
                  Marked(Grammar.Identifier.raw(")")),
                  Marked(Grammar.Identifier("listType")),
                ),
              ),
            ),
          ),
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("AssocNT")),
            nt = Grammar.StandardNonTerminal.`:`(
              reductions = NonEmptyList.nel(
                List(
                  Marked(Grammar.Identifier.raw("~")),
                  Marked(
                    Grammar.ListNonTerminal(
                      `type` = Grammar.ListNonTerminal.Type.+,
                      start = IgnoredList(
                        before = Nil,
                        unIgnored = Marked(Grammar.Identifier("AssocPair")),
                        after = Nil,
                      ),
                      repeat = IgnoredList(
                        before = List(
                          Marked(Grammar.Identifier.raw("|")),
                        ),
                        unIgnored = Marked(Grammar.Identifier("AssocPair")),
                        after = Nil,
                      ).some,
                    ),
                  ),
                  Marked(Grammar.Identifier("StandardNT")),
                ),
              ),
            ),
          ),
          Grammar.NT(
            name = Grammar.Identifier.unsafeNonTerminal(Marked("AssocPair")),
            nt = Grammar.StandardNonTerminal.`:`(
              reductions = NonEmptyList.nel(
                List(
                  Marked(Grammar.Identifier("assocDir")),
                  Marked(Grammar.Identifier("Element")),
                ),
              ),
            ),
          ),
        ),
      ),
    )

}
