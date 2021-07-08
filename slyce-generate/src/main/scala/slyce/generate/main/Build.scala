package slyce.generate.main

import java.util.UUID

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils.ado
import klib.utils._
import slyce.generate._
import slyce.generate.building._

object Build {

  def buildOutput(buildInput: BuildInput): Attempt[BuildOutput] = {
    type LexerItems = (Nfa, Dfa)
    type GrammarItems = (ExpandedGrammar)

    val lexerItems: Attempt[LexerItems] =
      for {
        nfa <- Nfa.fromLexer(buildInput.lexer)
        dfa <- Dfa.fromNfa(nfa)
      } yield (nfa, dfa)

    val grammarItems: Attempt[GrammarItems] =
      for {
        expandedGrammar <- ExpandedGrammar.fromGrammar(buildInput.grammar)
      } yield expandedGrammar

    for {
      joinedItems <- ado[Attempt].join(
        lexerItems,
        grammarItems,
      )
      ((nfa, dfa), expandedGrammar) = joinedItems
      deDuplicatedExpandedGrammar = ExpandedGrammar.simplifyAnonLists(expandedGrammar)
      parsingTable <- ParsingTable.fromExpandedGrammar(deDuplicatedExpandedGrammar)

      tokens = dfa.states.toList.flatMap {
        case (state, _) =>
          state.end.toList.flatMap { yields =>
            yields.yields.flatMap {
              _.value match {
                case Yields.Yield.Terminal(name, _) =>
                  name.some
                case _ =>
                  None
              }
            }
          }
      }.toSet

      egElements = deDuplicatedExpandedGrammar.nts.flatMap { nt =>
        nt.reductions.toList.flatMap(_.elements)
      }
      raws = egElements.flatMap {
        case ExpandedGrammar.Identifier.Raw(name) =>
          name.some
        case _ =>
          None
      }.toSet

      // TODO (KR) : Extra checks
      //           : - Unused,
    } yield BuildOutput(
      name = buildInput.name,
      nfa = nfa,
      dfa = dfa,
      tokens = tokens,
      raws = raws,
      expandedGrammar = expandedGrammar,
      deDuplicatedExpandedGrammar = deDuplicatedExpandedGrammar,
      parsingTable = parsingTable,
    )
  }

  def outputToString(
      `package`: List[String],
      output: BuildOutput,
  ): IndentedString = {
    import IndentedString._

    // =====|  |=====

    val anonListMap: Map[UUID, Int] =
      output.deDuplicatedExpandedGrammar.nts
        .flatMap {
          _.name match {
            case ExpandedGrammar.Identifier.NonTerminal.AnonListNt(key, _) => key.some
            case _                                                         => None
          }
        }
        .zipWithIndex
        .map { case (uuid, i) => (uuid, i + 1) }
        .toMap

    def rawName(raw: String): String =
      raw.unesc("`")

    def terminalName(terminal: String): String =
      terminal

    @tailrec
    def nonTerminalName(nonTerminal: ExpandedGrammar.Identifier.NonTerminal, prefix: String = ""): String =
      nonTerminal match {
        case ExpandedGrammar.Identifier.NonTerminal.NamedNt(name) =>
          s"$prefix$name"
        case ExpandedGrammar.Identifier.NonTerminal.ListNt(name, _type)    => s"$prefix$name${_type}"
        case ExpandedGrammar.Identifier.NonTerminal.AnonListNt(key, _type) => s"${prefix}AnonList${anonListMap(key)}${_type}"
        case ExpandedGrammar.Identifier.NonTerminal.AssocNt(name, idx)     => s"$prefix$name$idx"
        case ExpandedGrammar.Identifier.NonTerminal.AnonOptNt(identifier) =>
          identifier match {
            case ExpandedGrammar.Identifier.Terminal(name)           => s"${prefix}Opt_$name"
            case ExpandedGrammar.Identifier.Raw(name)                => s"${prefix}Opt_$name".unesc("`")
            case nonTerminal: ExpandedGrammar.Identifier.NonTerminal => nonTerminalName(nonTerminal, s"${prefix}Opt")
          }
      }

    def scopedIdentifierName(identifier: ExpandedGrammar.Identifier): String =
      identifier match {
        case ExpandedGrammar.Identifier.Terminal(name)           => s"Tok.${terminalName(name)}"
        case ExpandedGrammar.Identifier.Raw(name)                => s"Tok.${rawName(name)}"
        case nonTerminal: ExpandedGrammar.Identifier.NonTerminal => s"NonTerminal.${nonTerminalName(nonTerminal)}"
      }

    // =====|  |=====

    val headerNote: IndentedString =
      inline(
        "// !!! DO NOT MODIFY !!!",
        "// File was automatically generated by slyce",
        Break,
      )

    val packageHeader: IndentedString =
      if (`package`.isEmpty)
        inline()
      else
        inline(
          s"package ${`package`.mkString(".")}",
          Break,
        )

    val imports: IndentedString =
      inline(
        "import klib.Implicits._",
        "import klib.fp.types._",
        "import slyce.core._",
        "import slyce.parse._",
      )

    val body: IndentedString = {
      val tokens: IndentedString =
        inline(
          "sealed trait Tok extends Token",
          "object Tok {",
          indented(
            output.tokens.toList.map { tok =>
              s"final case class ${terminalName(tok)}(text: String, span: Span) extends Tok"
            },
            output.raws.nonEmpty ?
              inline(
                Break,
                output.raws.toList.map { raw =>
                  s"final case class ${rawName(raw)}(text: String, span: Span) extends Tok"
                },
                Break,
                "def findRawTerminal(text: String, span: Span): Attempt[Tok] =",
                indented(
                  "text match {",
                  indented(
                    output.raws.toList.map { raw =>
                      s"case ${raw.unesc} => ${rawName(raw)}(text, span).pure[Attempt]"
                    },
                    """case _ => Dead(Marked(s"Invalid raw-terminal : ${text.unesc}", span.some) :: Nil)""",
                  ),
                  "}",
                ),
              ) |
              inline(),
          ),
          "}",
        )

      val nts: IndentedString = {
        def typeSignature(name: String, reduction: ExpandedGrammar.NT.Reduction, `extends`: String): IndentedString =
          reduction.elements.nonEmpty ?
            inline(
              s"final case class $name(",
              indented(
                reduction.elements.zipWithIndex.map {
                  case (element, i) =>
                    s"_$i: ${scopedIdentifierName(element)},"
                },
              ),
              s") extends ${`extends`}",
            ) |
            inline(
              s"case object $name extends ${`extends`}",
            )

        inline(
          s"type NtRoot = NonTerminal.${output.deDuplicatedExpandedGrammar.startNt.value}",
          "sealed trait NonTerminal",
          "object NonTerminal {",
          indented(
            output.deDuplicatedExpandedGrammar.aliases.map {
              case (from, to) =>
                s"type ${nonTerminalName(from)} = ${nonTerminalName(to)}"
            },
            Break,
            output.deDuplicatedExpandedGrammar.nts.map { nt =>
              val ntName = nonTerminalName(nt.name)
              inline(
                (nt.reductions.size == 1) ?
                  inline(
                    typeSignature(ntName, nt.reductions.head, "NonTerminal"),
                  ) |
                  inline(
                    s"sealed trait $ntName extends NonTerminal",
                    s"object $ntName {",
                    indented(
                      nt.reductions.toList.zipWithIndex.map {
                        case (reduction, i) =>
                          typeSignature(s"_${i + 1}", reduction, ntName)
                      },
                    ),
                    "}",
                  ),
                Break,
              )
            },
          ),
          "}",
        )
      }

      val parser: IndentedString = {
        val lexer: IndentedString = {
          def makeState(idx: Int, state: slyce.generate.building.Dfa.State): IndentedString = {
            inline(
              s"lazy val s$idx: Lexer.State[Tok] =",
              indented(
                "Lexer.State[Tok](",
                indented(
                  s"$idx,",
                  inline(
                    "Map(",
                    indented(
                      state.transitions.toList
                        .flatMap {
                          case (chars, to) =>
                            val to2 = to.map(t => output.dfa.stateMap(t.value))
                            chars.toList.map((_, to2))
                        }
                        .sortBy(_._1)
                        .map {
                          case (c, to) =>
                            val charComment = s"// ${c.unesc}"
                            s"${c.toInt}.toChar -> ${to.cata(to => s"s$to.some, $charComment", "None,")}"
                        },
                    ),
                    "),",
                  ),
                  state.elseTransition.cata(to => s"s${output.dfa.stateMap(to.value)}.some,", "None,"),
                  state.end match {
                    case Some(yields) =>
                      inline(
                        "Lexer.Yields[Tok](",
                        indented(
                          "List(",
                          indented(
                            yields.yields.map { y =>
                              inline(
                                "Lexer.Yields.Yield[Tok](",
                                indented(
                                  s"${y.value.subString},",
                                  y.value match {
                                    case Yields.Yield.Text(_)           => s"Tok.findRawTerminal,"
                                    case Yields.Yield.Terminal(name, _) => s"Tok.$name(_, _).pure[Attempt]"
                                    case Yields.Yield.Const(text, _)    => s"Tok.${text.unesc("`")}(_, _).pure[Attempt]"
                                  },
                                ),
                                "),",
                              )
                            },
                          ),
                          "),",
                          yields.toMode.value match {
                            case Yields.ToMode.Same       => "Lexer.Yields.ToMode.Same,"
                            case Yields.ToMode.To(mode)   => s"Lexer.Yields.ToMode.To[Tok](s${output.dfa.stateMap(mode.value)}),"
                            case Yields.ToMode.Push(mode) => s"Lexer.Yields.ToMode.Push[Tok](s${output.dfa.stateMap(mode.value)}),"
                            case Yields.ToMode.Pop        => "Lexer.Yields.ToMode.Pop,"
                          },
                        ),
                        ").some,",
                      )
                    case None =>
                      s"None,"
                  },
                ),
                ")",
              ),
              Break,
            )
          }

          inline(
            "Lexer[Tok] {",
            indented(
              output.dfa.states.toList.map {
                case (state, i) =>
                  makeState(i, state)
              },
              "s0",
            ),
            "},",
          )
        }

        val grammar: IndentedString = {
          def makeState(state: ParsingTable.ParseState): IndentedString =
            inline(
              s"lazy val s${state.id}: Grammar.State[Tok, NonTerminal, NtRoot] =",
              indented(
                "Grammar.State[Tok, NonTerminal, NtRoot] {",
                indented(
                  "// --- Terminal Actions ---",
                  state.terminalActions.toList.map {
                    case (mTerm, termAction) =>
                      s"// ${mTerm.cata(scopedIdentifierName, "$")} => $termAction"
                  }.sorted,
                  Break,
                  "// --- NonTerminal Actions ---",
                  state.nonTerminalActions.toList.map {
                    case (nonTerminal, shift) =>
                      s"// ${nonTerminalName(nonTerminal)} => ${shift.to.value.id}"
                  }.sorted,
                  Break,
                  "// --- Finishes on ---",
                  state.finishesOn.toList.map(mt => s"// ${mt.cata(scopedIdentifierName, "$")}").sorted,
                  Break,
                  "??? // TODO : ...",
                ),
                "}",
              ),
              Break,
            )

          inline(
            "Grammar[Tok, NonTerminal, NtRoot] {",
            indented(
              output.parsingTable.states.map(makeState),
              s"s${output.parsingTable.startState.id}",
            ),
            "},",
          )
        }

        inline(
          "val parser: Parser[Tok, NonTerminal, NtRoot] =",
          indented(
            "Parser[Tok, NonTerminal, NtRoot](",
            indented(
              lexer,
              grammar,
            ),
            ")",
          ),
        )
      }

      inline(
        s"object ${output.name} {",
        indented(
          Break,
          tokens,
          Break,
          nts,
          Break,
          parser,
          Break,
        ),
        "}",
      )
    }

    // =====|  |=====

    inline(
      headerNote,
      packageHeader,
      imports,
      Break,
      body,
    )
  }

}
