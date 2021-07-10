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

    lazy val ntIsCollapsed: Map[ExpandedGrammar.Identifier.NonTerminal, Boolean] =
      output.deDuplicatedExpandedGrammar.nts.map { nt =>
        (
          nt.name,
          nt.reductions.size == 1,
        )
      }.toMap

    // TODO (KR) : Possibly improve (?)
    def unaliasNt(nt: ExpandedGrammar.Identifier.NonTerminal): ExpandedGrammar.Identifier.NonTerminal =
      output.deDuplicatedExpandedGrammar.aliases.find(_._1 == nt).toMaybe.cata(_._2, nt)

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

    def leftRightScopedIdentifierName(idx: Int, identifier: ExpandedGrammar.Identifier): String =
      identifier match {
        case nonTerminal: ExpandedGrammar.Identifier.NonTerminal => s"Right(_${idx + 1}: ${scopedIdentifierName(nonTerminal)})"
        case terminal: ExpandedGrammar.Identifier.Term           => s"Left(_${idx + 1}: ${scopedIdentifierName(terminal)})"
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
        "import klib.utils._",
        "import slyce.core._",
        "import slyce.parse._",
      )

    val body: IndentedString = {
      val quadQuote: String = '"'.toString * 4

      val tokens: IndentedString =
        inline(
          "sealed abstract class Tok(val tokName: String) extends Token",
          "object Tok {",
          indented(
            output.tokens.toList.map { tok =>
              s"final case class ${terminalName(tok)}(text: String, span: Span) extends Tok(${tok.unesc})"
            },
            output.raws.nonEmpty ?
              inline(
                Break,
                output.raws.toList.map { raw =>
                  s"final case class ${rawName(raw)}(text: String, span: Span) extends Tok(${raw.unesc("""""""""")})"
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
          def defineState(idx: Int): IndentedString =
            s"var s$idx: Lexer.State[Tok] = null"
          def makeState(idx: Int, state: slyce.generate.building.Dfa.State): IndentedString = {
            inline(
              s"s$idx =",
              indented(
                "Lexer.State[Tok](",
                indented(
                  s"$idx,",
                  if (state.transitions.nonEmpty)
                    inline(
                      "char => {",
                      indented(
                        "val int = char.toInt",
                        Break,
                        state.transitions.toList.zipWithIndex.map {
                          case ((chars, to), i1) =>
                            chars.groupChars.zipWithIndex.map {
                              case (charOrRange, i2) =>
                                val ifStmt = (i1 == 0 && i2 == 0) ? "if" | "else if"

                                inline(
                                  charOrRange match {
                                    case Left(char) =>
                                      s"$ifStmt (int == ${char.toInt}) // ${char.unesc}"
                                    case Right((charMin, charMax)) =>
                                      s"$ifStmt (int >= ${charMin.toInt} && int <= ${charMax.toInt}) // ${charMin.unesc}-${charMax.unesc}"
                                  },
                                  indented(
                                    to match {
                                      case Some(to) =>
                                        s"s${output.dfa.stateMap(to.value)}.some"
                                      case None =>
                                        "None"
                                    },
                                  ),
                                )
                            }
                        },
                        "else",
                        indented(
                          state.elseTransition match {
                            case Some(to) =>
                              s"s${output.dfa.stateMap(to.value)}.some"
                            case None =>
                              "None"
                          },
                        ),
                      ),
                      "},",
                    )
                  else
                    state.elseTransition match {
                      case Some(to) =>
                        s"_ => s${output.dfa.stateMap(to.value)},"
                      case None =>
                        "_ => None,"
                    },
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
                case (_, i) =>
                  defineState(i)
              },
              Break,
              output.dfa.states.toList.map {
                case (state, i) =>
                  makeState(i, state)
              },
              Break,
              "s0",
            ),
            "},",
          )
        }

        val grammar: IndentedString = {
          def defineState(state: ParsingTable.ParseState): IndentedString =
            s"var s${state.id}: Grammar.State[Tok, NonTerminal, NtRoot] = null"
          def makeState(state: ParsingTable.ParseState): IndentedString = {
            def makeReduce(reduce: ParsingTable.ParseState.Reduce, retToks: String): IndentedString = {
              val ParsingTable.ParseState.Reduce((pNt, pIdx), rIdentifiers) = reduce
              val ntName = s"${scopedIdentifierName(pNt)}${ntIsCollapsed(unaliasNt(pNt)) ? "" | s"._${pIdx + 1}"}"

              rIdentifiers.toNel match {
                case Some(rIdentifiers) =>
                  val ntRef = s"$ntName${1.to(rIdentifiers.size).map(i => s"_$i").mkString("(", ", ", ")")}"
                  val zipped = rIdentifiers.reverse.zipWithIndex
                  val head = s"(${leftRightScopedIdentifierName(zipped.head._2, zipped.head._1)}, poppedState)"
                  val tail = zipped.tail.map {
                    case (identifier, i) =>
                      s"(${leftRightScopedIdentifierName(i, identifier)}, _)"
                  }
                  val matchStr = (head :: tail).reverse.mkString(" :: ")
                  inline(
                    // REMOVE : ...
                    /*
                    "// REMOVE : ...",
                    s"""println("reducing ($ntRef)".toColorString.red)""",
                    s"""println("    ${state.id}")""",
                    """stack.foreach{s => println { s"    ${s._1 match { case Left(tok) => tok.tokName; case Right(nt) => nt.getClass }} @ ${s._2.id}" } }""",
                     */
                    //
                    "stack match {",
                    indented(
                      s"case $matchStr :: stack =>",
                      indented(
                        s"val nt: NonTerminal = $ntRef",
                        "poppedState.onNt(nt) match {",
                        indented(
                          "case Alive(to) =>",
                          indented(
                            "(",
                            indented(
                              "to,",
                              "(nt.right, poppedState) :: stack,",
                              s"$retToks,",
                            ),
                            ").left.pure[Attempt]",
                          ),
                          "case dead @ Dead(_) =>",
                          indented(
                            "dead",
                          ),
                        ),
                        "}",
                      ),
                      "case _ =>",
                      indented(s"""Dead(Marked("This should be impossible (1)... $matchStr") :: Nil)"""),
                    ),
                    "}",
                  )
                case None =>
                  // TODO (KR) : Might need changes here as well...
                  inline(
                    s"val nt: NonTerminal = $ntName",
                    s"s${state.id}.onNt(nt) match {",
                    indented(
                      "case Alive(to) =>",
                      indented(
                        "(",
                        indented(
                          "to,",
                          "stack,",
                          "None,",
                        ),
                        ").left.pure[Attempt]",
                      ),
                      "case dead @ Dead(_) =>",
                      indented(
                        "dead",
                      ),
                    ),
                    "}",
                  )
              }
            }

            // TODO (KR) :
            inline(
              s"s${state.id} =",
              indented(
                "Grammar.State[Tok, NonTerminal, NtRoot](",
                indented(
                  s"${state.id},",
                  "{ (stack, tokens) =>",
                  indented(
                    "tokens match {",
                    indented(
                      "case Some(tokens) =>",
                      indented(
                        "tokens.head match {",
                        indented(
                          state.terminalActions.toList
                            .flatMap {
                              case (on, action) =>
                                on.map((_, action))
                            }
                            .map {
                              case (term, action) =>
                                inline(
                                  s"case tok: ${scopedIdentifierName(term)} =>",
                                  indented(
                                    action match {
                                      case ParsingTable.ParseState.Shift(to) =>
                                        inline(
                                          "(",
                                          indented(
                                            s"s${to.value.id},",
                                            s"(tok.left, s${state.id}) :: stack,",
                                            "tokens.tail.toNel,",
                                          ),
                                          ").left.pure[Attempt]",
                                        )
                                      case reduce @ ParsingTable.ParseState.Reduce(_, _) =>
                                        makeReduce(reduce, "tokens.some")
                                    },
                                  ),
                                )
                            },
                          "case tok =>",
                          indented(
                            """Dead(Marked("Unexpected token", tok.span.some) :: Nil)""",
                          ),
                        ),
                        "}",
                      ),
                      "case None =>",
                      indented(
                        state.terminalActions.get(None).toMaybe match {
                          case Some(terminalAction) =>
                            inline(
                              terminalAction match {
                                case reduce @ ParsingTable.ParseState.Reduce(_, _) =>
                                  makeReduce(reduce, "None")
                                case ParsingTable.ParseState.Shift(_) =>
                                  "??? // NOTE : It should not be possible to generate this..."
                              },
                            )
                          case None =>
                            if (state.finishesOn.contains(None)) {
                              inline(
                                "stack match {",
                                indented(
                                  "case (Right(ntRoot: NtRoot), _) :: Nil =>",
                                  indented("ntRoot.right.pure[Attempt]"),
                                  "case _ =>",
                                  indented("""Dead(Marked("This should be impossible (2)...") :: Nil)"""),
                                ),
                                "}",
                              )
                            } else {
                              """Dead(Marked("Unexpected EOF") :: Nil)"""
                            }
                        },
                      ),
                    ),
                    "}",
                  ),
                  "},",
                  "{",
                  indented(
                    state.nonTerminalActions.toList.map {
                      case (nonTerminal, shift) =>
                        inline(
                          s"case _: ${scopedIdentifierName(nonTerminal)} =>",
                          indented(s"s${shift.to.value.id}.pure[Attempt]"),
                        )
                    },
                    "case _ =>",
                    indented("""Dead(Marked("This should be impossible (3)...") :: Nil)"""),
                  ),
                  "},",
                ),
                ")",
              ),
              Break,
            )
          }

          inline(
            "Grammar[Tok, NonTerminal, NtRoot] {",
            indented(
              output.parsingTable.states.map(defineState),
              Break,
              output.parsingTable.states.map(makeState),
              s"s${output.parsingTable.startState.id}",
            ),
            "},",
          )
        }

        inline(
          // TODO (KR) : Remove `lazy` (?)
          "lazy val parser: Parser[Tok, NonTerminal, NtRoot] =",
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
