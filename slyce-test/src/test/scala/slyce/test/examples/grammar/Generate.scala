package slyce.test.examples.grammar

import klib.Implicits._
import klib.fp.types._
import klib.utils._

import slyce.core._
import slyce.generate._
import slyce.generate.input._
import slyce.generate.main._
import slyce.test.examples._

object Generate {
  import Marked.Implicits._

  private object T {
    val nonTerminal = Grammar.Identifier.unsafeTerminal("nonTerminal".marked)
    val terminal = Grammar.Identifier.unsafeTerminal("terminal".marked)
    val mode = Grammar.Identifier.unsafeTerminal("mode".marked)
    val chars = Grammar.Identifier.unsafeTerminal("chars".marked)
    val escChar = Grammar.Identifier.unsafeTerminal("escChar".marked)
    val listType = Grammar.Identifier.unsafeTerminal("listType".marked)
    val tilde = Grammar.Identifier.unsafeTerminal("tilde".marked)
    val assocType = Grammar.Identifier.unsafeTerminal("assocType".marked)

    val `@start:` = Grammar.Identifier.raw("@start:").marked
    val `:` = Grammar.Identifier.raw(":").marked
    val `^` = Grammar.Identifier.raw("^").marked
    val `|` = Grammar.Identifier.raw("|").marked
    val `;` = Grammar.Identifier.raw(";").marked
    val `?` = Grammar.Identifier.raw("?").marked
    val `"` = Grammar.Identifier.raw("\"").marked
    val `.` = Grammar.Identifier.raw(".").marked
    val `(` = Grammar.Identifier.raw("(").marked
    val `)` = Grammar.Identifier.raw(")").marked
  }

  val executable: Executable =
    debugGenerate(
      BuildInput(
        "grammar",
        lexer = {
          import Lexer.Mode
          import Mode.Line
          import Regex._
          import CharClass._

          object M {
            val General = "General".marked
            val Mode = "Mode".marked
            val String = "String".marked
          }

          Lexer(
            startMode = M.General,
            modes = List(
              Mode(
                name = M.General,
                lines = List(
                  Line(
                    priority = 5,
                    regex = Sequence(
                      inclusive('/'),
                      inclusive('/'),
                      exclusive('\n').anyAmount,
                      inclusive('\n'),
                    ).marked,
                    yields = Yields(
                      yields = Nil,
                    ),
                  ),
                  Line(
                    priority = 6,
                    regex = Sequence(
                      inclusive('/'),
                      inclusive('*'),
                      Group(
                        Sequence(exclusive('*')),
                        Sequence(inclusive('*'), exclusive('/')),
                      ).anyAmount,
                      inclusive('*'),
                      inclusive('/'),
                    ).marked,
                    yields = Yields(
                      yields = Nil,
                    ),
                  ),
                  Line(
                    priority = 7,
                    regex = inclusive(' ', '\t', '\n').atLeastOnce.marked,
                    yields = Yields(
                      yields = Nil,
                    ),
                  ),
                  Line(
                    priority = 8,
                    regex = Sequence("@start:").marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                      toMode = M.Mode.map(Yields.ToMode.Push(_)),
                    ),
                  ),
                  Line(
                    priority = 9,
                    regex = Sequence(
                      `[A-Z]`,
                      `[A-Za-z_\\d]`.anyAmount,
                    ).marked,
                    yields = Yields(
                      yields = List(
                        T.nonTerminal.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                  Line(
                    priority = 10,
                    regex = Sequence(
                      `[a-z]`,
                      `[A-Za-z_\\d]`.anyAmount,
                    ).marked,
                    yields = Yields(
                      yields = List(
                        T.terminal.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                  Line(
                    priority = 11,
                    regex = inclusive('"').marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                      toMode = M.String.map(Yields.ToMode.Push(_)),
                    ),
                  ),
                  Line(
                    priority = 12,
                    regex = inclusive('*', '+').marked,
                    yields = Yields(
                      yields = List(
                        T.listType.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                  Line(
                    priority = 13,
                    regex = inclusive('~').marked,
                    yields = Yields(
                      yields = List(
                        T.tilde.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                  Line(
                    priority = 14,
                    regex = inclusive('<', '>').marked,
                    yields = Yields(
                      yields = List(
                        T.assocType.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                ),
              ),
              Mode(
                name = M.Mode,
                lines = List(
                  Line(
                    priority = 17,
                    regex = inclusive(' ', '\t').atLeastOnce.marked,
                    yields = Yields(
                      yields = Nil,
                    ),
                  ),
                  Line(
                    priority = 18,
                    regex = Sequence(
                      `[A-Z]`,
                      `[A-Za-z_\\d]`.anyAmount,
                    ).marked,
                    yields = Yields(
                      yields = List(
                        T.mode.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                  Line(
                    priority = 19,
                    regex = inclusive('\n').marked,
                    yields = Yields(
                      yields = Nil,
                      toMode = Yields.ToMode.Pop.marked,
                    ),
                  ),
                ),
              ),
              Mode(
                name = M.String,
                lines = List(
                  Line(
                    priority = 22,
                    regex = inclusive('"').marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                      toMode = Yields.ToMode.Pop.marked,
                    ),
                  ),
                  Line(
                    priority = 23,
                    regex = exclusive('\\').atLeastOnce.marked,
                    yields = Yields(
                      yields = List(
                        T.chars.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                  Line(
                    priority = 24,
                    regex = Sequence(
                      inclusive('\\'),
                      inclusive('\\', 'n', 't', '"'),
                    ).marked,
                    yields = Yields(
                      yields = List(
                        T.escChar.map(t => Yields.Yield.Terminal(t.name, (1.some, 1.some))),
                      ),
                    ),
                  ),
                ),
              ),
            ),
          )
        },
        grammar = {

          object NT {
            val _Grammar = Grammar.Identifier.unsafeNonTerminal("Grammar".marked)
            val ElementList = Grammar.Identifier.unsafeNonTerminal("ElementList".marked)
            val UnIgnoredElementList = Grammar.Identifier.unsafeNonTerminal("UnIgnoredElementList".marked)
            val NonTerminal = Grammar.Identifier.unsafeNonTerminal("NonTerminal".marked)
            val NTBody = Grammar.Identifier.unsafeNonTerminal("NTBody".marked)
            val StandardNT = Grammar.Identifier.unsafeNonTerminal("StandardNT".marked)
            val BasicNT = Grammar.Identifier.unsafeNonTerminal("BasicNT".marked)
            val LiftNT = Grammar.Identifier.unsafeNonTerminal("LiftNT".marked)
            val ListNT = Grammar.Identifier.unsafeNonTerminal("ListNT".marked)
            val AssocNT = Grammar.Identifier.unsafeNonTerminal("AssocNT".marked)
            val AssocPair = Grammar.Identifier.unsafeNonTerminal("AssocPair".marked)
            val Element = Grammar.Identifier.unsafeNonTerminal("Element".marked)
            val AnonList = Grammar.Identifier.unsafeNonTerminal("AnonList".marked)
            val NonOptElement = Grammar.Identifier.unsafeNonTerminal("NonOptElement".marked)
            val Raw = Grammar.Identifier.unsafeNonTerminal("Raw".marked)
            val Char = Grammar.Identifier.unsafeNonTerminal("Char".marked)
          }

          Grammar(
            startNt = NT._Grammar.map(_.name),
            nts = List(
              Grammar.NT(
                name = NT._Grammar,
                nt = Grammar.StandardNonTerminal.`:`.simple(
                  T.`@start:`,
                  T.mode,
                  Grammar.ListNonTerminal.Type.+.repeat(
                    IgnoredList.builder
                      .unIgnored(NT.NonTerminal)
                      .after(T.`;`)
                      .build,
                  ).marked,
                ),
              ),
              Grammar.NT(
                name = NT.ElementList,
                nt = Grammar.ListNonTerminal.Type.*.simple(NT.Element),
              ),
              Grammar.NT(
                name = NT.UnIgnoredElementList,
                nt = Grammar.StandardNonTerminal.`:`(
                  List(
                    NT.Element,
                  ),
                  List(
                    NT.ElementList,
                    T.`^`,
                    NT.Element,
                    NT.ElementList,
                  ),
                ),
              ),
              Grammar.NT(
                name = NT.NonTerminal,
                nt = Grammar.StandardNonTerminal.`:`.simple(
                  T.nonTerminal,
                  NT.NTBody,
                ),
              ),
              Grammar.NT(
                name = NT.NTBody,
                nt = Grammar.StandardNonTerminal.^.simple(
                  NT.StandardNT,
                  NT.ListNT,
                  NT.AssocNT,
                ),
              ),
              Grammar.NT(
                name = NT.StandardNT,
                nt = Grammar.StandardNonTerminal.^.simple(
                  NT.BasicNT,
                  NT.LiftNT,
                ),
              ),
              Grammar.NT(
                name = NT.BasicNT,
                nt = Grammar.StandardNonTerminal.`:`.simple(
                  T.`:`,
                  Grammar.ListNonTerminal.Type.+.simpleBetween(
                    NT.ElementList,
                    T.`|`,
                  ).marked,
                ),
              ),
              Grammar.NT(
                name = NT.LiftNT,
                nt = Grammar.StandardNonTerminal.`:`.simple(
                  T.`^`,
                  Grammar.ListNonTerminal.Type.+.simpleBetween(
                    NT.UnIgnoredElementList,
                    T.`|`,
                  ).marked,
                ),
              ),
              Grammar.NT(
                name = NT.ListNT,
                nt = Grammar.StandardNonTerminal.`:`(
                  List(
                    T.listType,
                    NT.UnIgnoredElementList,
                  ),
                  List(
                    T.listType,
                    NT.UnIgnoredElementList,
                    T.`.`,
                    NT.UnIgnoredElementList,
                  ),
                ),
              ),
              Grammar.NT(
                name = NT.AssocNT,
                nt = Grammar.StandardNonTerminal.`:`.simple(
                  T.tilde,
                  Grammar.ListNonTerminal.Type.+.simpleBetween(
                    NT.AssocPair,
                    T.`|`,
                  ).marked,
                ),
              ),
              Grammar.NT(
                name = NT.AssocPair,
                nt = Grammar.StandardNonTerminal.`:`.simple(
                  T.assocType,
                  NT.Element,
                ),
              ),
              Grammar.NT(
                name = NT.Element,
                nt = Grammar.StandardNonTerminal.`:`.simple(
                  NT.NonOptElement,
                  T.`?`.map(Grammar.Optional),
                ),
              ),
              Grammar.NT(
                name = NT.AnonList,
                nt = Grammar.StandardNonTerminal.`:`(
                  List(
                    NT.Element,
                    T.listType,
                  ),
                  List(
                    T.`(`,
                    NT.UnIgnoredElementList,
                    T.`)`,
                    T.listType,
                  ),
                  List(
                    T.`(`,
                    NT.UnIgnoredElementList,
                    T.`.`,
                    NT.UnIgnoredElementList,
                    T.`)`,
                    T.listType,
                  ),
                ),
              ),
              Grammar.NT(
                name = NT.NonOptElement,
                nt = Grammar.StandardNonTerminal.^.simple(
                  T.nonTerminal,
                  T.terminal,
                  NT.Raw,
                  NT.AnonList,
                ),
              ),
              Grammar.NT(
                name = NT.Raw,
                nt = Grammar.StandardNonTerminal.`:`.simple(
                  T.`"`,
                  Grammar.ListNonTerminal.Type.+.simple(NT.Char).marked,
                  T.`"`,
                ),
              ),
              Grammar.NT(
                name = NT.Char,
                nt = Grammar.StandardNonTerminal.^.simple(
                  T.chars,
                  T.escChar,
                ),
              ),
            ),
          )
        },
      ),
    )

}
