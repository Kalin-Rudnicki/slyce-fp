package slyce.test.examples.lexer

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

  val executable: Executable = {

    object T {
      val char = Grammar.Identifier.unsafeTerminal("char".marked)
      val chars = Grammar.Identifier.unsafeTerminal("chars".marked)
      val escChar = Grammar.Identifier.unsafeTerminal("escChar".marked)
      val escChars = Grammar.Identifier.unsafeTerminal("escChars".marked)
      val int = Grammar.Identifier.unsafeTerminal("int".marked)
      val mode = Grammar.Identifier.unsafeTerminal("mode".marked)
      val term = Grammar.Identifier.unsafeTerminal("term".marked)

      val `@start:` = Grammar.Identifier.raw("@start:").marked
      val `@mode:` = Grammar.Identifier.raw("@mode:").marked
      val `;` = Grammar.Identifier.raw(";").marked
      val `,` = Grammar.Identifier.raw(",").marked
      val `@` = Grammar.Identifier.raw("@").marked
      val `[` = Grammar.Identifier.raw("[").marked
      val `]` = Grammar.Identifier.raw("]").marked
      val `>>` = Grammar.Identifier.raw(">>").marked
      val `->` = Grammar.Identifier.raw("->").marked
      val `<-` = Grammar.Identifier.raw("<-").marked
      val `\"` = Grammar.Identifier.raw("\"").marked
      val `|` = Grammar.Identifier.raw("|").marked
      val `(` = Grammar.Identifier.raw("(").marked
      val `)` = Grammar.Identifier.raw(")").marked
      val `{` = Grammar.Identifier.raw("{").marked
      val `}` = Grammar.Identifier.raw("}").marked
      val `^` = Grammar.Identifier.raw("^").marked
      val `-` = Grammar.Identifier.raw("-").marked
      val `?` = Grammar.Identifier.raw("?").marked
      val `*` = Grammar.Identifier.raw("*").marked
      val `+` = Grammar.Identifier.raw("+").marked
      val `_` = Grammar.Identifier.raw("").marked
    }

    debugGenerate(
      BuildInput(
        name = "lexer",
        lexer = {
          import Lexer.Mode
          import Mode.Line
          import Regex._
          import CharClass._

          object M {
            val General = "General".marked
            val Mode = "Mode".marked
            val String = "String".marked
            val LineEnd = "LineEnd".marked
            val CharClass = "CharClass".marked
            val Quant = "Quant".marked
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
                      inclusive('@'),
                      Group(
                        Sequence("start"),
                        Sequence("mode"),
                      ),
                      inclusive(':'),
                    ).marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                      toMode = M.Mode.map(Yields.ToMode.Push(_)),
                    ),
                  ),
                  Line(
                    priority = 6,
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
                    priority = 7,
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
                    priority = 8,
                    regex = Sequence(
                      inclusive('\\'),
                      inclusive('.', '/', '@', ';', 'n', 't', '\\', '[', ']', '(', ')', '{', '}', '?', '*', '+'),
                    ).marked,
                    yields = Yields(
                      yields = List(
                        T.escChar.map(t => Yields.Yield.Terminal(t.name, (1.some, 1.some))),
                      ),
                    ),
                  ),
                  Line(
                    priority = 9,
                    regex = inclusive('\n').marked,
                    yields = Yields(
                      yields = Nil,
                    ),
                  ),
                  Line(
                    priority = 10,
                    regex = inclusive(';').marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                      toMode = M.LineEnd.map(Yields.ToMode.Push(_)),
                    ),
                  ),
                  Line(
                    priority = 11,
                    regex = inclusive('(', ')', '?', '*', '+').marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                    ),
                  ),
                  Line(
                    priority = 12,
                    regex = inclusive('[').marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                      toMode = M.CharClass.map(Yields.ToMode.Push(_)),
                    ),
                  ),
                  Line(
                    priority = 13,
                    regex = inclusive('{').marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                      toMode = M.Quant.map(Yields.ToMode.Push(_)),
                    ),
                  ),
                  Line(
                    priority = 14,
                    regex = Sequence("\\d").marked,
                    yields = Yields(
                      yields = List(
                        T.escChars.map(t => Yields.Yield.Terminal(t.name, (1.some, 1.some))),
                      ),
                    ),
                  ),
                  Line(
                    priority = 15,
                    regex = exclusive('\\').marked,
                    yields = Yields(
                      yields = List(
                        T.char.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                  Line(
                    priority = 16,
                    regex = inclusive('.').marked,
                    yields = Yields(
                      yields = List(
                        T.escChars.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                  Line(
                    priority = 16,
                    regex = inclusive(' ').repeat(4, None).marked,
                    yields = Yields(
                      yields = Nil,
                    ),
                  ),
                ),
              ),
              Mode(
                name = M.Mode,
                lines = List(
                  Line(
                    priority = 20,
                    regex = inclusive(' ', '\t').atLeastOnce.marked,
                    yields = Yields(
                      yields = Nil,
                    ),
                  ),
                  Line(
                    priority = 21,
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
                    priority = 22,
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
                    priority = 25,
                    regex = inclusive('"').marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                      toMode = Yields.ToMode.Pop.marked,
                    ),
                  ),
                  Line(
                    priority = 26,
                    regex = exclusive('\\', '"').atLeastOnce.marked,
                    yields = Yields(
                      yields = List(
                        T.chars.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                  Line(
                    priority = 27,
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
              Mode(
                name = M.LineEnd,
                lines = List(
                  Line(
                    priority = 30,
                    regex = inclusive(' ', '\t').atLeastOnce.marked,
                    yields = Yields(
                      yields = Nil,
                    ),
                  ),
                  Line(
                    priority = 31,
                    regex = Sequence(
                      inclusive('/'),
                      inclusive('/'),
                      exclusive('\n').anyAmount,
                      inclusive('\n'),
                    ).marked,
                    yields = Yields(
                      yields = Nil,
                      toMode = Yields.ToMode.Pop.marked,
                    ),
                  ),
                  Line(
                    priority = 32,
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
                    priority = 33,
                    regex = Group(
                      Sequence(inclusive('[', ']', ',', '@')),
                      Sequence(">>"),
                      Sequence("->"),
                      Sequence("<-"),
                    ).marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                    ),
                  ),
                  Line(
                    priority = 34,
                    regex = Sequence(
                      inclusive('?').maybe,
                      `\\d`.atLeastOnce,
                    ).marked,
                    yields = Yields(
                      yields = List(
                        T.int.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                  Line(
                    priority = 35,
                    regex = inclusive('"').marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                      toMode = M.String.map(Yields.ToMode.Push(_)),
                    ),
                  ),
                  Line(
                    priority = 36,
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
                    priority = 37,
                    regex = Sequence(
                      `[a-z]`,
                      `[A-Za-z_\\d]`.anyAmount,
                    ).marked,
                    yields = Yields(
                      yields = List(
                        T.term.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                  Line(
                    priority = 38,
                    regex = inclusive('\n').marked,
                    yields = Yields(
                      yields = Nil,
                      toMode = Yields.ToMode.Pop.marked,
                    ),
                  ),
                ),
              ),
              Mode(
                name = M.CharClass,
                lines = List(
                  Line(
                    priority = 41,
                    regex = inclusive(']').marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                      toMode = Yields.ToMode.Pop.marked,
                    ),
                  ),
                  Line(
                    priority = 42,
                    regex = inclusive('^', '-').marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                    ),
                  ),
                  Line(
                    priority = 43,
                    regex = Sequence(
                      inclusive('\\'),
                      inclusive('\\', ']', '^', 'n', 't', '-'),
                    ).marked,
                    yields = Yields(
                      yields = List(
                        T.escChar.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                  Line(
                    priority = 44,
                    regex = Sequence("\\d").marked,
                    yields = Yields(
                      yields = List(
                        T.escChars.map(t => Yields.Yield.Terminal(t.name, (1.some, 1.some))),
                      ),
                    ),
                  ),
                  Line(
                    priority = 45,
                    regex = exclusive('\\').marked,
                    yields = Yields(
                      yields = List(
                        T.char.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                ),
              ),
              Mode(
                name = M.Quant,
                lines = List(
                  Line(
                    priority = 48,
                    regex = inclusive(' ', '\t').atLeastOnce.marked,
                    yields = Yields(
                      yields = Nil,
                    ),
                  ),
                  Line(
                    priority = 49,
                    regex = Sequence(
                      inclusive('?').maybe,
                      `\\d`.atLeastOnce,
                    ).marked,
                    yields = Yields(
                      yields = List(
                        T.int.map(t => Yields.Yield.Terminal(t.name)),
                      ),
                    ),
                  ),
                  Line(
                    priority = 50,
                    regex = inclusive(',').marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                    ),
                  ),
                  Line(
                    priority = 51,
                    regex = inclusive('}').marked,
                    yields = Yields(
                      yields = List(
                        Yields.Yield.Text().marked,
                      ),
                      toMode = Yields.ToMode.Pop.marked,
                    ),
                  ),
                ),
              ),
            ),
          )
        },
        grammar = {

          object NT {
            val _Lexer = Grammar.Identifier.unsafeNonTerminal("Lexer".marked)
            val Mode = Grammar.Identifier.unsafeNonTerminal("Mode".marked)
            val Line = Grammar.Identifier.unsafeNonTerminal("Line".marked)
            val Yield = Grammar.Identifier.unsafeNonTerminal("Yield".marked)
            val YieldType = Grammar.Identifier.unsafeNonTerminal("YieldType".marked)
            val SubString = Grammar.Identifier.unsafeNonTerminal("SubString".marked)
            val ToMode = Grammar.Identifier.unsafeNonTerminal("ToMode".marked)
            val Raw = Grammar.Identifier.unsafeNonTerminal("Raw".marked)
            val Char = Grammar.Identifier.unsafeNonTerminal("Char".marked)
            val Regex = Grammar.Identifier.unsafeNonTerminal("Regex".marked)
            val GroupInner = Grammar.Identifier.unsafeNonTerminal("GroupInner".marked)
            val Sequence = Grammar.Identifier.unsafeNonTerminal("Sequence".marked)
            val Group = Grammar.Identifier.unsafeNonTerminal("Group".marked)
            val Quant = Grammar.Identifier.unsafeNonTerminal("Quant".marked)
            val CharClass = Grammar.Identifier.unsafeNonTerminal("CharClass".marked)
            val CCChars = Grammar.Identifier.unsafeNonTerminal("CCChars".marked)
            val CCChar = Grammar.Identifier.unsafeNonTerminal("CCChar".marked)
          }

          Grammar(
            startNt = NT._Lexer.map(_.name),
            nts = List(
              Grammar.NT(
                name = NT._Lexer,
                nt = Grammar.StandardNonTerminal.`:`.simple(
                  T.`@start:`,
                  T.mode,
                  Grammar.ListNonTerminal.Type.+.simple(NT.Mode).marked,
                ),
              ),
              Grammar.NT(
                name = NT.Mode,
                nt = Grammar.StandardNonTerminal.`:`.simple(
                  T.`@mode:`,
                  T.mode,
                  Grammar.ListNonTerminal.Type.+.simple(NT.Line).marked,
                ),
              ),
              Grammar.NT(
                name = NT.Line,
                nt = Grammar.StandardNonTerminal.`:`.simple(
                  NT.GroupInner,
                  T.`;`,
                  Grammar.ListNonTerminal.Type.*.simpleBetween(NT.Yield, T.`,`).marked,
                  NT.ToMode.map(Grammar.Optional),
                ),
              ),
              Grammar.NT(
                name = NT.Yield,
                nt = Grammar.StandardNonTerminal.`:`.simple(
                  NT.YieldType,
                  NT.SubString,
                ),
              ),
              Grammar.NT(
                name = NT.YieldType,
                nt = Grammar.StandardNonTerminal.^.simple(
                  T.`@`,
                  T.term,
                  NT.Raw,
                ),
              ),
              Grammar.NT(
                name = NT.SubString,
                nt = Grammar.StandardNonTerminal.`:`(
                  List(
                    T.`[`,
                    T.int.map(Grammar.Optional),
                    T.`]`,
                  ),
                  List(
                    T.`[`,
                    T.int.map(Grammar.Optional),
                    T.`,`,
                    T.int.map(Grammar.Optional),
                    T.`]`,
                  ),
                ),
              ),
              Grammar.NT(
                name = NT.ToMode,
                nt = Grammar.StandardNonTerminal.`:`(
                  List(
                    T.`>>`,
                    T.mode,
                  ),
                  List(
                    T.`->`,
                    T.mode,
                  ),
                  List(
                    T.`<-`,
                  ),
                ),
              ),
              Grammar.NT(
                name = NT.Raw,
                nt = Grammar.StandardNonTerminal.^.apply(
                  IgnoredList.builder
                    .before(T.`\"`)
                    .unIgnored(Grammar.ListNonTerminal.Type.+.simple(NT.Char).marked)
                    .after(T.`\"`)
                    .build,
                ),
              ),
              Grammar.NT(
                name = NT.Char,
                nt = Grammar.StandardNonTerminal.^.simple(
                  T.chars,
                  T.escChar,
                ),
              ),
              Grammar.NT(
                name = NT.Regex,
                nt = Grammar.StandardNonTerminal.`:`(
                  List(
                    NT.Group,
                  ),
                  List(
                    NT.CharClass,
                  ),
                  List(
                    NT.Regex,
                    NT.Quant,
                  ),
                ),
              ),
              Grammar.NT(
                name = NT.GroupInner,
                nt = Grammar.ListNonTerminal.Type.+.simpleBetween(NT.Sequence, T.`|`),
              ),
              Grammar.NT(
                name = NT.Sequence,
                nt = Grammar.ListNonTerminal.Type.*.simple(NT.Regex),
              ),
              Grammar.NT(
                name = NT.Group,
                nt = Grammar.StandardNonTerminal.^.apply(
                  IgnoredList.builder
                    .before(T.`(`)
                    .unIgnored(NT.GroupInner)
                    .after(T.`)`)
                    .build,
                ),
              ),
              Grammar.NT(
                name = NT.Quant,
                nt = Grammar.StandardNonTerminal.`:`(
                  List(
                    T.`?`,
                  ),
                  List(
                    T.`*`,
                  ),
                  List(
                    T.`+`,
                  ),
                  List(
                    T.`{`,
                    T.int.map(Grammar.Optional),
                    T.`}`,
                  ),
                  List(
                    T.`{`,
                    T.int.map(Grammar.Optional),
                    T.`,`,
                    T.int.map(Grammar.Optional),
                    T.`}`,
                  ),
                ),
              ),
              Grammar.NT(
                name = NT.CharClass,
                nt = Grammar.StandardNonTerminal.`:`(
                  List(
                    T.`[`,
                    T.`^`.map(Grammar.Optional),
                    Grammar.ListNonTerminal.Type.+.simple(NT.CCChars).marked,
                    T.`]`,
                  ),
                  List(
                    T.char,
                  ),
                  List(
                    T.escChar,
                  ),
                  List(
                    T.escChars,
                  ),
                ),
              ),
              Grammar.NT(
                name = NT.CCChars,
                nt = Grammar.StandardNonTerminal.`:`(
                  List(
                    NT.CCChar,
                    T.`-`,
                    NT.CCChar,
                  ),
                  List(
                    NT.CCChar,
                  ),
                  List(
                    T.escChars,
                  ),
                ),
              ),
              Grammar.NT(
                name = NT.CCChar,
                nt = Grammar.StandardNonTerminal.^.simple(
                  T.char,
                  T.escChar,
                ),
              ),
            ),
          )
        },
      ),
    )
  }

}
