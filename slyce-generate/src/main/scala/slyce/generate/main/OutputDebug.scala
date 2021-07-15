package slyce.generate.main

import java.io.File

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils.ado
import klib.utils._

import slyce.core._
import slyce.generate._
import slyce.generate.input._
import slyce.generate.building._

object OutputDebug {

  private val DebugOutputDir: File = new File("target/slyce/debug")

  def outputDebug(
      buildInput: BuildInput,
      aBuildOutput: PartialBuildOutput \/ BuildOutput,
      debutOutputDir: Maybe[File],
  ): IO[Unit] = {
    import scalatags.Text.all.{name => _, _}
    def htmlFrag: Frag = {

      // =====| Css |=====

      val CssSettings = scalacss.devOrProdDefaults
      import CssSettings._

      object C {

        def apply(classes: String*): String =
          classes.mkString(" ")

        val internal = "internal"
        val page = "page"
        val section = "section"
        val subSection = "sub-section"
        val setting = "setting"

      }

      object MyStyles extends StyleSheet.Standalone {
        import dsl._

        "table, th, td" - (
          border := "1px solid black",
        )

        "a" - (
          textDecoration := "none",
          color := "black",
          backgroundColor := "#90C3C8",
          borderRadius := "5px",
        )
        "a:hover" - (
          backgroundColor := "#1F5673"
        )

        "li" - (
          height := "fit-content",
        )

        ".section-header" - (
          margin := "0px",
        )

        ".internal" - (
          marginLeft := "25px",
        )
        ".page" - (
          marginTop := "25px",
          marginLeft := "25px",
        )
        ".section" - (
          borderLeft := "3px solid green",
          padding := "10px",
          &("h2") - (
            margin := "0px",
          ),
        )
        ".sub-section" - (
          borderLeft := "3px solid blue",
          padding := "10px",
          &("h3") - (
            margin := "0px",
          ),
        )
        ".setting" - (
          borderLeft := "3px solid red",
          padding := "10px",
          &("h4") - (
            margin := "0px",
          ),
        )

      }

      // =====| Helpers |=====

      val TODO = h3("TODO", color := "red")

      // ...

      def page(header: String)(_body: Frag*): Frag =
        body(`class` := C.page)(
          h1(header),
          div(`class` := C.internal)(
            _body: _*,
          ),
        )

      def section(header: String)(body: Frag*): Frag =
        div(`class` := C.section)(
          h2(header),
          br,
          div(`class` := C.internal)(
            body: _*,
          ),
        )

      def subSection(header: String)(body: Frag*): Frag =
        div(`class` := C.subSection)(
          h3(header),
          br,
          div(`class` := C.internal)(
            body: _*,
          ),
        )

      def setting(header: String)(body: Frag*): Frag =
        div(`class` := C.setting)(
          h4(header),
          br,
          div(`class` := C.internal)(
            body: _*,
          ),
        )

      def sectionOrErrors[T](t: Attempt[T])(onSuccess: T => Frag): Frag =
        t match {
          case Alive(r) => onSuccess(r)
          case Dead(errors) =>
            messagesToHtml("Error(s)", errors)
        }

      // =====| Sections |=====

      def messagesToHtml(
          title: String,
          messages: List[Marked[Msg]],
      ): Frag = {
        def convertMessage(msg: Marked[Msg]): Frag = {

          tr(
            td(msg.value.toString, whiteSpace := "pre"),
            td(msg.toString(false)),
          )
        }

        section(
          s"$title [${messages.size}]:",
        )(
          table(
            tr(
              th("Message")(
                width := "600px",
              ),
              th("Span")(
                width := "200px",
              ),
            ),
            messages.map(convertMessage),
          ),
        )
      }

      def inputToHtml(
          buildInput: BuildInput,
      ): Frag = {
        def lexerToHtml(lexer: Lexer): Frag = {

          subSection(
            "Lexer",
          )(
            setting("StartMode")(
              p(lexer.startMode.value),
            ),
            br,
            setting("Modes")(
              table(
                tr(
                  th("Priority")(
                    width := "100px",
                  ),
                  th("Regex")(
                    width := "750px",
                  ),
                  th("Yields")(
                    width := "350px",
                  ),
                  th("ToMode")(
                    width := "150px",
                  ),
                ),
                lexer.modes.map { mode =>
                  tr(
                    td(b(mode.name.value))(
                      colspan := 4,
                      textAlign := "center",
                    ),
                  ) ::
                    List[Frag](
                      mode.lines.map { line =>
                        tr(
                          td(line.priority),
                          td(line.regex.value.toString),
                          td(
                            ul(
                              line.yields.yields.map { y =>
                                li(y.value.toString)
                              },
                            ),
                          ),
                          td(line.yields.toMode.value.toString),
                        )
                      },
                    )
                },
              ),
            ),
          )
        }

        def grammarToHtml(grammar: Grammar): Frag = {
          def ntToHtml(nt: Grammar.NonTerminal): Frag = {
            def elementListToHtml(elements: List[Marked[Grammar.Element]]): Frag = {

              ul(
                elements.map { re =>
                  li(elementToHtml(re.value))
                },
              )
            }

            def ignoredListToHtml(iList: IgnoredList[Marked[Grammar.Element]]): Frag = {

              ul(
                li(
                  "Before",
                  elementListToHtml(iList.before),
                ),
                li(
                  "UnIgnored",
                  elementListToHtml(iList.unIgnored :: Nil),
                ),
                li(
                  "After",
                  elementListToHtml(iList.after),
                ),
              )
            }

            nt match {
              case standardNT: Grammar.StandardNonTerminal =>
                standardNT match {
                  case Grammar.StandardNonTerminal.`:`(reductions) =>
                    div(
                      b("StandardNonTerminal.`:`"),
                      ol(
                        reductions.toList.map { reduction =>
                          li(
                            elementListToHtml(reduction),
                          )
                        },
                      ),
                    )
                  case Grammar.StandardNonTerminal.^(reductions) =>
                    div(
                      b("StandardNonTerminal.`^`"),
                      ol(
                        reductions.toList.map { reduction =>
                          li(
                            ignoredListToHtml(reduction),
                          )
                        },
                      ),
                    )
                }
              case Grammar.ListNonTerminal(_type, start, repeat) =>
                div(
                  b(s"ListNonTerminal.${_type}"),
                  ul(
                    li(
                      "Start",
                      ignoredListToHtml(start),
                    ),
                    repeat.map { il =>
                      li(
                        "Repeat",
                        ignoredListToHtml(il),
                      )
                    }.toList,
                  ),
                )
              case Grammar.AssocNonTerminal(assocs, base) =>
                div(
                  b("AssocNonTerminal"),
                  ul(
                    li(
                      "Assocs",
                      ol(
                        assocs.toList.map {
                          case (aType, element) =>
                            li(
                              ul(
                                li(aType.value.toString),
                                li(elementToHtml(element.value)),
                              ),
                            )
                        },
                      ),
                    ),
                    li(
                      "Base",
                      ntToHtml(base),
                    ),
                  ),
                )
            }
          }
          def elementToHtml(element: Grammar.Element): Frag = {

            val (isOpt, elem) = element.toNonOpt
            if (isOpt)
              div(
                b("Optional"),
                elementToHtml(elem),
              )
            else
              elem match {
                case identifier: Grammar.Identifier =>
                  identifier.toString
                case lnt: Grammar.ListNonTerminal =>
                  ntToHtml(lnt)
              }
          }

          subSection(
            "Grammar",
          )(
            setting("StartNt")(
              p(grammar.startNt.value),
            ),
            br,
            setting(s"Nts (${grammar.nts.size})")(
              table(
                tr(
                  th("Name")(
                    width := "150px",
                  ),
                  th("NonTerminal")(
                    width := "fit-content",
                  ),
                ),
                grammar.nts.map { nt =>
                  tr(
                    td(nt.name.value.toString),
                    td(ntToHtml(nt.nt)),
                  )
                },
              ),
            ),
          )
        }

        section(
          "BuildInput",
        )(
          lexerToHtml(buildInput.lexer),
          br,
          grammarToHtml(buildInput.grammar),
        )
      }

      def outputToHtml: Frag = {
        def nfaToHtml(nfa: Attempt[Nfa]): Frag = {
          subSection("Nfa")(
            sectionOrErrors(nfa) { nfa =>
              def stateId(idx: Int): Frag =
                a(
                  span(
                    idx,
                    padding := "3px 10px",
                    margin := "5px 0px",
                  ),
                  id := s"nfa-state-$idx",
                )
              def stateRef(idx: Int): Frag =
                a(
                  span(
                    idx,
                    padding := "3px 10px",
                    margin := "5px 0px",
                  ),
                  href := s"#nfa-state-$idx",
                )

              val allNfaStates: Set[Nfa.State] =
                findAll(nfa.modes.toList.map(_._2.value.value).toSet) { state =>
                  state.transition.map(_._2.value).toSet |
                    state.epsilonTransitions.map(_.value)
                }

              val nfaStateMap = allNfaStates.toList.zipWithIndex.toMap

              List[Frag](
                setting("Modes")(
                  table(
                    tr(
                      th("Name")(
                        width := "150px",
                      ),
                      th("State")(
                        width := "150px",
                      ),
                    ),
                    nfa.modes.toList.map {
                      case (name, mpns) =>
                        tr(
                          td(name),
                          td(
                            stateRef(nfaStateMap(mpns.value.value)),
                            textAlign := "center",
                          ),
                        )
                    },
                  ),
                ),
                br,
                setting("States")(
                  table(
                    tr(
                      th("Id", width := "50px"),
                      th("Transition", width := "200px"),
                      th("Epsilon Transitions", width := "150px"),
                      th("Yields", width := "150px"),
                      th("ToMode", width := "125px"),
                    ),
                    nfaStateMap.toList.sortBy(_._2).map {
                      case (state, idx) =>
                        tr(
                          td(stateId(idx), verticalAlign := "top", textAlign := "center"),
                          td(
                            state.transition.map {
                              case (cc, ts) =>
                                ul(
                                  li(cc.toString),
                                  li(stateRef(nfaStateMap(ts.value))),
                                )
                            }.toList,
                          ),
                          td(
                            ul(
                              state.epsilonTransitions.toList
                                .map(sp => nfaStateMap(sp.value))
                                .sorted
                                .map(i => li(stateRef(i))),
                            ),
                          ),
                          td(
                            ul(
                              state.end.toList
                                .flatMap(_.yields.yieldsTerminals)
                                .sorted
                                .map(li(_)),
                            ),
                          ),
                          td(state.end.map(_.yields.toMode.value.toString).toList),
                        )
                    },
                  ),
                ),
              )
            },
          )
        }

        def dfaToHtml(dfa: Attempt[Dfa]): Frag =
          subSection("Dfa")(
            sectionOrErrors(dfa) { dfa =>
              def stateId(idx: Int): Frag =
                a(
                  span(
                    idx,
                    padding := "3px 10px",
                    margin := "5px 0px",
                  ),
                  id := s"dfa-state-$idx",
                )
              def stateRef(idx: Int): Frag =
                a(
                  span(
                    idx,
                    padding := "3px 10px",
                    margin := "5px 0px",
                  ),
                  href := s"#dfa-state-$idx",
                )

              List[Frag](
                setting("Modes")(
                  table(
                    tr(
                      th("Name")(
                        width := "150px",
                      ),
                      th("State")(
                        width := "150px",
                      ),
                    ),
                    dfa.modeStarts.toList.map {
                      case (name, dfaState) =>
                        tr(
                          td(name),
                          td(stateRef(dfaState.id)),
                        )
                    },
                  ),
                ),
                br,
                setting("States")(
                  table(
                    tr(
                      th("State")(
                        width := "150px",
                      ),
                      th("Transitions")(
                        width := "fit-content",
                      ),
                      th("ElseTransition")(
                        width := "150px",
                      ),
                      th("Yields")(
                        width := "350px",
                      ),
                      th("ToMode")(
                        width := "150px",
                      ),
                    ),
                    dfa.states.toList.sortBy(_.id).map {
                      state =>
                        tr(
                          td(stateId(state.id), verticalAlign := "top", textAlign := "center"),
                          td(
                            table(
                              tr(
                                th("On")(
                                  width := "250px",
                                ),
                                th("ToState")(
                                  width := "150px",
                                ),
                              ),
                              state.transitions.toList.map {
                                case (on, toState) =>
                                  tr(
                                    td(on.prettyChars),
                                    td(
                                      toState.map { ts =>
                                        stateRef(ts.value.id)
                                      }.toList,
                                      textAlign := "center",
                                    ),
                                  )
                              },
                            )(
                              border := "none",
                            ),
                          ),
                          td(
                            state.elseTransition.map { e =>
                              stateRef(e.value.id)
                            }.toList,
                            textAlign := "center",
                          ),
                          td(
                            ul(
                              state.end.toList.flatMap { e =>
                                e.yields.map { y =>
                                  li(y.value.toString)
                                }
                              },
                            ),
                          ),
                          td(
                            state.end.toList.map { e =>
                              {
                                e.toMode.value match {
                                  case Yields.ToMode.To(mode)   => List[Frag](s"To: ", stateRef(mode.value.id))
                                  case Yields.ToMode.Push(mode) => List[Frag](s"Push: ", stateRef(mode.value.id))
                                  case Yields.ToMode.Same       => "Same"
                                  case Yields.ToMode.Pop        => "Pop"
                                }
                              }: Frag
                            },
                          ),
                        )
                    },
                  ),
                ),
              )
            },
          )

        def expandedGrammarToHtml(label: String, expandedGrammar: Attempt[ExpandedGrammar]): Frag =
          subSection(label)(
            sectionOrErrors(expandedGrammar) { expandedGrammar =>
              List[Frag](
                setting("StartNt")(
                  p(expandedGrammar.startNt.value),
                ),
                br,
                setting(s"Nts (${expandedGrammar.nts.size})")(
                  table(
                    tr(
                      th("Reductions")(
                        width := "350px",
                      ),
                      th("LiftIdx")(
                        width := "150px",
                      ),
                    ),
                    expandedGrammar.nts.sortBy(_.name.toString).map { nt =>
                      List[Frag](
                        tr(
                          td(nt.name.toString)(
                            colspan := 2,
                          )(
                            textAlign := "center",
                          ),
                        ),
                        nt.reductions.toList.map { r =>
                          tr(
                            td(
                              div(
                                ul(
                                  r.elements.map { e =>
                                    li(e.toString)
                                  },
                                ),
                              )(
                                minHeight := "25px",
                              ),
                            ),
                            td(
                              r.liftIdx.toList,
                            ),
                          )
                        },
                      )
                    },
                  ),
                ),
                br,
                setting(s"Aliases (${expandedGrammar.aliases.size})")(
                  table(
                    tr(
                      th("Identifier")(
                        width := "250px",
                      ),
                      th("References")(
                        width := "250px",
                      ),
                    ),
                    expandedGrammar.aliases.map {
                      case ExpandedGrammar.Alias(named, actual) =>
                        tr(
                          td(named.toString),
                          td(actual.toString),
                        )
                    },
                  ),
                ),
                br,
                setting("Withs")(
                  table(
                    tr(
                      th("Identifier", width := "200px"),
                      th("NT", width := "200px"),
                      th("Name", width := "200px"),
                    ),
                    expandedGrammar.withs.map {
                      case ExpandedGrammar.With(identifier, nt, name) =>
                        tr(
                          td(identifier.toString),
                          td(nt.toString),
                          td(name),
                        )
                    },
                  ),
                ),
                br,
                setting("Extras")(
                  ul(
                    expandedGrammar.extras.map(e => li(e.toString)),
                  ),
                ),
              )
            },
          )

        def parsingTableToHtml(parsingTable: Attempt[ParsingTable]): Frag =
          subSection("Parsing Table")(
            sectionOrErrors(parsingTable) { parsingTable =>
              val finishTerms =
                parsingTable.states.flatMap { state => state.finishesOn.map(_.cata(_.toString, "$")) }

              val otherTerms =
                parsingTable.states.flatMap { state => state.terminalActions.toList.map(_._1.cata(_.toString, "$")) }

              val allTerminals =
                (finishTerms ::: otherTerms).distinct.sorted

              val allNonTerminals =
                parsingTable.states.flatMap { state => state.nonTerminalActions.toList.map(_._1.toString) }.distinct.sorted

              def terminalIdx(term: Maybe[ExpandedGrammar.Identifier.Term]): Int =
                allTerminals.indexOf(term.cata(_.toString, "$"))
              def nonTerminalIdx(nonTerm: ExpandedGrammar.Identifier.NonTerminal): Int =
                allTerminals.size + allNonTerminals.indexOf(nonTerm.toString)

              List[Frag](
                subSection("Parsing Table")(
                  setting("Start State")(
                    p(parsingTable.startState.id),
                  ),
                  setting("Table")(
                    table(
                      tr(
                        th(colspan := allTerminals.size)("Terminals"),
                        th(colspan := allNonTerminals.size)("NonTerminals"),
                      ),
                      tr(
                        allTerminals.map(t => th(padding := "0 25px", minWidth := "70px")(t)),
                        allNonTerminals.map(t => th(padding := "0 25px", minWidth := "70px")(t)),
                      ),
                      parsingTable.states.map { state =>
                        tr(
                          td(s"State #${state.id}"),
                          td(TODO),
                        )
                      },
                    ),
                  ),
                ),
              )
            },
          )

        def cata[T](good: BuildOutput => T, bad: PartialBuildOutput => Attempt[T]): Attempt[T] =
          aBuildOutput match {
            case Right(buildOutput)       => good(buildOutput).pure[Attempt]
            case Left(partialBuildOutput) => bad(partialBuildOutput)
          }

        section(
          "BuildOutput",
        )(
          nfaToHtml(cata(_.nfa, _.nfa)),
          br,
          dfaToHtml(cata(_.dfa, _.dfa)),
          br,
          expandedGrammarToHtml("ExpandedGrammar", cata(_.expandedGrammar, _.expandedGrammar)),
          br,
          expandedGrammarToHtml("DeDuplicated ExpandedGrammar", cata(_.deDuplicatedExpandedGrammar, _.deDuplicatedExpandedGrammar)),
          br,
          parsingTableToHtml(cata(_.parsingTable, _.parsingTable)),
        )
      }

      // =====| Usage |=====

      html(
        head(
          tag("style")(MyStyles.render),
        ),
        page(
          s"Debug output for: ${buildInput.name}",
        )(
          messagesToHtml("Error(s)", Nil), // TODO (KR) :
          br,
          inputToHtml(buildInput),
          br,
          outputToHtml,
        ),
      )
    }

    val doDir = debutOutputDir.getOrElse(DebugOutputDir)
    for {
      _ <- doDir.mkdirs.pure[IO]
      outputFile = new File(doDir, s"${buildInput.name}.html")
      htmlText = htmlFrag.render
      _ <- IO.writeFile(outputFile, htmlText)
    } yield ()
  }

}
