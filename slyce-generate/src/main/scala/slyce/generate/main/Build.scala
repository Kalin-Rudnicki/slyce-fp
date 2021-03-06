package slyce.generate.main

import java.util.UUID

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils.ado
import klib.utils._

import slyce.core._
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

      lexerDefines = dfa.states.toList.flatMap { state =>
        state.end.toList.flatMap { end =>
          end.yields.flatMap { yields =>
            yields.value match {
              case Yields.Yield.Terminal(name, _) => name.some
              case _                              => None
            }
          }
        }
      }.toSet

      _ = lexerDefines.toList.sorted.foreach(println)

      grammarDefines = deDuplicatedExpandedGrammar.nts.map(_.name).toSet
      ntReferences = deDuplicatedExpandedGrammar.nts.flatMap { nt =>
        nt.reductions.toList.zipWithIndex.flatMap {
          case (r, i1) =>
            r.elements.zipWithIndex.map {
              case (e, i2) =>
                (nt.name, i1, i2, e)
            }
        }
      }
      grammarNTReferences = ntReferences.flatMap {
        case (inNt, ri, rei, nt: ExpandedGrammar.Identifier.NonTerminal) =>
          (inNt, ri, rei, nt).some
        case _ =>
          None
      }
      grammarTReferences = ntReferences.flatMap {
        case (inNt, ri, rei, t: ExpandedGrammar.Identifier.Terminal) =>
          (inNt, ri, rei, t).some
        case _ =>
          None
      }
      grammarRawReferences = ntReferences.flatMap {
        case (inNt, ri, rei, r: ExpandedGrammar.Identifier.Raw) =>
          (inNt, ri, rei, r).some
        case _ =>
          None
      }

      // TODO (KR) : Checks
      //           : [ERROR] Lexer toMode to DNE mode (possibly already checked)
      //           : [ERROR] Lexer line is completely overshadowed by other lines
      //           : [WARN ] Lexer defines unreferenced terminal
      //           : [?????] Defining Raw/Terminal/NonTerminal with name known to cause problems
      //           :         Create a list, possibly 2 levels, 1 that can be `...`'ed and fixed, and one that cant

      // TODO (KR) : Possibly get marked data as well?
      grammarReferencesDneTerminal: Attempt[Unit] = {
        def checkReference(
            inNt: ExpandedGrammar.Identifier.NonTerminal,
            reductionIdx: Int,
            elementInReduction: Int,
            t: ExpandedGrammar.Identifier.Terminal,
        ): Attempt[Unit] =
          if (lexerDefines.contains(t.name))
            ().pure[Attempt]
          else
            Dead(Marked(Msg(s"Grammar references non-existent terminal: $inNt[$reductionIdx][$elementInReduction] = $t")) :: Nil)

        grammarTReferences
          .map((checkReference _).tupled)
          .traverse
          .map(_ => ())
      }

      // TODO (KR) : Possibly get marked data as well?
      grammarReferencesDneNonTerminal: Attempt[Unit] = {
        def checkReference(
            inNt: ExpandedGrammar.Identifier.NonTerminal,
            reductionIdx: Int,
            elementInReduction: Int,
            nt: ExpandedGrammar.Identifier.NonTerminal,
        ): Attempt[Unit] =
          if (grammarDefines.contains(nt))
            ().pure[Attempt]
          else
            Dead(Marked(Msg(s"Grammar references non-existent non-terminal: $inNt[$reductionIdx][$elementInReduction] = $nt")) :: Nil)

        grammarNTReferences
          .map((checkReference _).tupled)
          .traverse
          .map(_ => ())
      }

      _ <- ado[Attempt].join(
        grammarReferencesDneTerminal,
        grammarReferencesDneNonTerminal,
      )

      parsingTable <- ParsingTable.fromExpandedGrammar(deDuplicatedExpandedGrammar)
    } yield BuildOutput(
      name = buildInput.name,
      nfa = nfa,
      dfa = dfa,
      tokens = grammarTReferences.map(_._4).toSet,
      raws = grammarRawReferences.map(_._4).toSet,
      expandedGrammar = expandedGrammar,
      deDuplicatedExpandedGrammar = deDuplicatedExpandedGrammar,
      parsingTable = parsingTable,
    )
  }

  // =====|  |=====

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

    val ntIsCollapsed: ExpandedGrammar.Identifier.NonTerminal => Boolean = {
      val map: Map[ExpandedGrammar.Identifier.NonTerminal, Boolean] =
        output.deDuplicatedExpandedGrammar.nts.map { nt =>
          (
            nt.name,
            nt.reductions.size == 1,
          )
        }.toMap

      map.getOrElse(_, false)
    }

    val ntIsCaseObject: ExpandedGrammar.Identifier.NonTerminal => Boolean = {
      val map: Map[ExpandedGrammar.Identifier.NonTerminal, Boolean] =
        output.deDuplicatedExpandedGrammar.nts.map { nt =>
          (
            nt.name,
            nt.reductions.size == 1 && nt.reductions.head.elements.isEmpty,
          )
        }.toMap

      map.getOrElse(_, false)
    }

    val withsByIdentifier: ExpandedGrammar.Identifier => List[ExpandedGrammar.With] = {
      val map: Map[ExpandedGrammar.Identifier, List[ExpandedGrammar.With]] =
        output.deDuplicatedExpandedGrammar.withs.groupBy(_.identifier)

      map.getOrElse(_, Nil)
    }

    val extrasByNonTerminal: ExpandedGrammar.Identifier.NonTerminal => List[ExpandedGrammar.Extra] = {
      val map: Map[ExpandedGrammar.Identifier.NonTerminal, List[ExpandedGrammar.Extra]] =
        output.deDuplicatedExpandedGrammar.extras.groupMap(_.nt)(_.extra)

      map.getOrElse(_, Nil)
    }

    val withsByNonTerminal: ExpandedGrammar.Identifier.NonTerminal => Map[String, List[ExpandedGrammar.With]] = {
      val map: Map[String, Map[String, List[ExpandedGrammar.With]]] =
        output.deDuplicatedExpandedGrammar.withs
          .groupBy(_.nt.toString)
          .map {
            case (k, v) =>
              (
                k,
                v.groupBy(_.name),
              )
          }

      // TODO (KR) : possible unaliasing issues
      nt => map.getOrElse(nt.toString, Map.empty)
    }

    def withName(`with`: ExpandedGrammar.With): String =
      s"${scopedIdentifierName(`with`.nt, false)}.${`with`.name}"

    def identifierWithString(identifier: ExpandedGrammar.Identifier): String =
      withsByIdentifier(identifier).flatMap { w =>
        (withsByNonTerminal(w.nt)(w.name).size > 1).maybe(s" with ${withName(w)}")
      }.mkString

    def rawName(raw: ExpandedGrammar.Identifier.Raw): String =
      raw.name.unesc("`")

    def terminalName(terminal: ExpandedGrammar.Identifier.Terminal): String =
      terminal.name

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

    def scopedIdentifierName(identifier: ExpandedGrammar.Identifier, includeType: Boolean): String =
      identifier match {
        case terminal: ExpandedGrammar.Identifier.Terminal => s"Tok.${terminalName(terminal)}"
        case raw: ExpandedGrammar.Identifier.Raw           => s"Tok.${rawName(raw)}"
        case nonTerminal: ExpandedGrammar.Identifier.NonTerminal =>
          s"NonTerminal.${nonTerminalName(nonTerminal)}${(includeType && ntIsCaseObject(nonTerminal)) ? ".type" | ""}"
      }

    def leftRightScopedIdentifierName(idx: Int, identifier: ExpandedGrammar.Identifier): String =
      identifier match {
        case nonTerminal: ExpandedGrammar.Identifier.NonTerminal => s"Right(_${idx + 1}: ${scopedIdentifierName(nonTerminal, true)})"
        case terminal: ExpandedGrammar.Identifier.Term           => s"Left(_${idx + 1}: ${scopedIdentifierName(terminal, true)})"
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
        "import scala.annotation.tailrec",
        Break,
        "import klib.Implicits._",
        "import klib.fp.types._",
        "import klib.utils._",
        Break,
        "import slyce.core._",
        "import slyce.parse._",
      )

    val body: IndentedString = {
      val tokens: IndentedString =
        inline(
          "sealed abstract class Tok(val tokName: String) extends Token",
          "object Tok {",
          indented(
            output.tokens.toList
              .map(t => (t, terminalName(t)))
              .sortBy(_._2)
              .map {
                case (tok, tokName) =>
                  s"final case class $tokName(text: String, span: Span) extends Tok(${tok.name.unesc})${identifierWithString(tok)}"
              },
            output.raws.nonEmpty ?
              inline(
                Break,
                output.raws.toList
                  .map(r => (r, rawName(r)))
                  .sortBy(_._2)
                  .map {
                    case (raw, rawName) =>
                      s"final case class $rawName(text: String, span: Span) extends Tok(${raw.name.unesc("""""""""")})${identifierWithString(raw)}"
                  },
                Break,
                "def findRawTerminal(text: String, span: Span): Attempt[Tok] =",
                indented(
                  "text match {",
                  indented(
                    output.raws.toList
                      .map(r => (r, rawName(r)))
                      .sortBy(_._2)
                      .map {
                        case (raw, rawName) =>
                          s"case ${raw.name.unesc} => $rawName(text, span).pure[Attempt]"
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
                    s"_$i: ${scopedIdentifierName(element, true)},"
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
              case ExpandedGrammar.Alias(from, actual) =>
                s"type ${nonTerminalName(from)} = ${nonTerminalName(actual)}"
            },
            Break,
            output.deDuplicatedExpandedGrammar.nts
              .map(nt => (nt, nonTerminalName(nt.name)))
              .sortBy(_._2)
              .map {
                case (nt, ntName) =>
                  val withs = withsByNonTerminal(nt.name)

                  val withIdtStr =
                    inline(
                      withs.toList.map {
                        case (name, withs) =>
                          withs match {
                            case head :: Nil =>
                              s"type $name = ${scopedIdentifierName(head.identifier, true)}"
                            case _ =>
                              s"sealed trait $name"
                          }
                      },
                    )

                  val extras = extrasByNonTerminal(nt.name)
                  val (extrasBracket, extrasBody) =
                    if (extras.isEmpty)
                      ("", inline())
                    else
                      (
                        " {",
                        inline(
                          indented(
                            Break,
                            extras.map {
                              extra =>
                                def makeLoop(ntName2: String, liftIdx: Int, tailIdx: Int): IndentedString =
                                  inline(
                                    "@tailrec",
                                    s"def loop(queue: $ntName2, stack: List[$ntName.${ExpandedGrammar.LiftType}]): List[$ntName.${ExpandedGrammar.LiftType}] =",
                                    indented(
                                      "queue match {",
                                      indented(
                                        s"case head: $ntName2._1 => loop(head._$tailIdx, head._$liftIdx :: stack)",
                                        s"case _: $ntName2._2.type => stack.reverse",
                                      ),
                                      "}",
                                    ),
                                  )

                                inline(
                                  extra match {
                                    case ExpandedGrammar.Extra.SimpleToList(liftIdx, tailIdx) =>
                                      inline(
                                        s"def toList: List[$ntName.${ExpandedGrammar.LiftType}] = {",
                                        indented(
                                          makeLoop(ntName, liftIdx, tailIdx),
                                          Break,
                                          "loop(this, Nil)",
                                        ),
                                        "}",
                                      )
                                    case ExpandedGrammar.Extra.HeadTailToList(isNel, headLiftIdx, headTailIdx, tailNt, tailLiftIdx, tailTailIdx) =>
                                      if (isNel)
                                        inline(
                                          s"def toNonEmptyList: NonEmptyList[$ntName.${ExpandedGrammar.LiftType}] = {",
                                          indented(
                                            makeLoop(nonTerminalName(tailNt), tailLiftIdx, tailTailIdx),
                                            Break,
                                            s"NonEmptyList[$ntName.${ExpandedGrammar.LiftType}](this._$headLiftIdx, loop(this._$headTailIdx, Nil))",
                                          ),
                                          "}",
                                        )
                                      else
                                        inline(
                                          s"def toList: List[$ntName.${ExpandedGrammar.LiftType}] = {",
                                          indented(
                                            makeLoop(nonTerminalName(tailNt), tailLiftIdx, tailTailIdx),
                                            Break,
                                            "this match {",
                                            indented(
                                              s"case head: $ntName._1 => head._$headLiftIdx :: loop(head._$headTailIdx, Nil)",
                                              s"case _: $ntName._2.type => Nil",
                                            ),
                                            "}",
                                          ),
                                          "}",
                                        )
                                    case ExpandedGrammar.Extra.Optional =>
                                      inline(
                                        s"def toMaybe: Maybe[$ntName.${ExpandedGrammar.LiftType}] =",
                                        indented(
                                          "this match {",
                                          indented(
                                            s"case $ntName._1(some) => some.some",
                                            s"case $ntName._2 => None",
                                          ),
                                          "}",
                                        ),
                                      )
                                    case ExpandedGrammar.Extra.Lift(idxs) =>
                                      inline(
                                        s"def lift: $ntName.${ExpandedGrammar.LiftType} =",
                                        indented(
                                          "this match {",
                                          indented(
                                            idxs.toList.zipWithIndex.map {
                                              case (liftIdx, rIdx) =>
                                                inline(
                                                  s"case nt: $ntName._${rIdx + 1} => nt._$liftIdx",
                                                )
                                            },
                                          ),
                                          "}",
                                        ),
                                      )
                                  },
                                  Break,
                                )
                            },
                          ),
                          "}",
                        ),
                      )

                  inline(
                    if (nt.reductions.size == 1)
                      inline(
                        typeSignature(ntName, nt.reductions.head, s"NonTerminal${identifierWithString(nt.name)}$extrasBracket"),
                        extrasBody,
                        withs.nonEmpty.maybe {
                          inline(
                            s"object $ntName {",
                            indented(
                              withIdtStr,
                            ),
                            "}",
                          )
                        },
                      )
                    else
                      inline(
                        s"sealed trait $ntName extends NonTerminal${identifierWithString(nt.name)}$extrasBracket",
                        extrasBody,
                        s"object $ntName {",
                        indented(
                          withs.nonEmpty.maybe {
                            inline(
                              withIdtStr,
                              Break,
                            )
                          },
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
          def defineState(state: Dfa.State): IndentedString =
            s"var s${state.id}: Lexer.State[Tok] = null"
          def makeState(state: Dfa.State): IndentedString = {
            inline(
              s"s${state.id} =",
              indented(
                "Lexer.State[Tok](",
                indented(
                  s"${state.id},",
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
                                        s"s${to.value.id}.some"
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
                              s"s${to.value.id}.some"
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
                        s"_ => s${to.value.id},"
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
                            case Yields.ToMode.To(mode)   => s"Lexer.Yields.ToMode.To[Tok](s${mode.value.id}),"
                            case Yields.ToMode.Push(mode) => s"Lexer.Yields.ToMode.Push[Tok](s${mode.value.id}),"
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
              output.dfa.states.toList.map { state =>
                defineState(state)
              },
              Break,
              output.dfa.states.toList.map { state =>
                makeState(state)
              },
              Break,
              s"s${output.dfa.states.head.id}",
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
              val ntName = s"${scopedIdentifierName(pNt, false)}${ntIsCollapsed(pNt) ? "" | s"._${pIdx + 1}"}"

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
                          s"(nt.right, s${state.id}) :: stack,",
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
                                  s"case tok: ${scopedIdentifierName(term, true)} =>",
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
                          s"case _: ${scopedIdentifierName(nonTerminal, true)} =>",
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
