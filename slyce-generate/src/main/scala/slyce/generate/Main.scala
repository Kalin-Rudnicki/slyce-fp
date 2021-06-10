package slyce.generate

import java.io.File

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils.ado
import klib.utils._
import slyce.core._
import slyce.generate._
import input._
import building._

object Main {

  final case class BuildInput(
      lexer: Lexer,
      grammar: Grammar,
  )

  final case class BuildOutput(
      nfa: Nfa,
      dfa: Dfa,
      expandedGrammar: ExpandedGrammar,
      expandedGrammar2: ExpandedGrammar,
  )

  def build(buildInput: BuildInput): Attempt[BuildOutput] = {
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

      // TODO (KR) : Extra checks
    } yield BuildOutput(
      nfa = nfa,
      dfa = dfa,
      expandedGrammar = expandedGrammar,
      expandedGrammar2 = ExpandedGrammar.simplifyAnonLists(expandedGrammar),
    )
  }

  private val DebugOutputDir: File = new File("target/slyce/debug")

  def outputDebug(
      name: String,
      buildInput: BuildInput,
      aBuildOutput: Attempt[BuildOutput],
  ): IO[Unit] = {
    import scalatags.Text.all.{name => _, _}

    // =====| Css |=====

    val CssSettings = scalacss.devOrProdDefaults
    import CssSettings._

    object MyStyles extends StyleSheet.Standalone {
      import dsl._

      "table, th, td" - (
        border := "1px solid black",
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

    object C {

      def apply(classes: String*): String =
        classes.mkString(" ")

      val internal = "internal"
      val page = "page"
      val section = "section"
      val subSection = "sub-section"
      val setting = "setting"

    }

    // =====| Helpers |=====

    val TODO = h3("TODO", color := "red")

    // ...

    def page(header: String)(_body: Frag*): Frag =
      body(
        h1(header),
        div(
          _body: _*,
        )(
          `class` := C.internal,
        ),
      )(
        `class` := C.page,
      )

    def section(header: String)(body: Frag*): Frag =
      div(
        h2(header),
        br,
        div(
          body: _*,
        )(
          `class` := C.internal,
        ),
      )(
        `class` := C.section,
      )

    def subSection(header: String)(body: Frag*): Frag =
      div(
        h3(header),
        br,
        div(
          body: _*,
        )(
          `class` := C.internal,
        ),
      )(
        `class` := C.subSection,
      )

    def setting(header: String)(body: Frag*): Frag =
      div(
        h4(header),
        br,
        div(
          body: _*,
        )(
          `class` := C.internal,
        ),
      )(
        `class` := C.setting,
      )

    // =====| Sections |=====

    def messagesToHtml(
        title: String,
        messages: List[Marked[Msg]],
    ): Frag = {
      def convertMessage(msg: Marked[Msg]): Frag = {

        tr(
          td(msg.value.toString),
          td(msg.span.map(_.toString).toList),
        )
      }

      section(
        s"$title [${messages.size}]:",
      )(
        table(
          tr(
            th("Message")(
              width := "350px",
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

    def outputToHtml(
        buildOutput: BuildOutput,
    ): Frag = {
      def dfaToHtml(dfa: Dfa): Frag = {

        subSection("Dfa")(
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
                    td(dfa.stateId(dfaState)),
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
              dfa.states.toList.map {
                case (state, _) =>
                  tr(
                    td(dfa.stateId(state)),
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
                                  dfa.stateId(ts.value)
                                }.toList,
                              ),
                            )
                        },
                      )(
                        border := "none",
                      ),
                    ),
                    td(
                      state.elseTransition.map { e =>
                        dfa.stateId(e.value)
                      }.toList,
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
                        e.toMode.value.map(ls => dfa.stateId(ls.value)).toString
                      },
                    ),
                  )
              },
            ),
          ),
        )
      }

      def expandedGrammarToHtml(expandedGrammar: ExpandedGrammar): Frag =
        subSection("ExpandedGrammar")(
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
              expandedGrammar.nts.map { nt =>
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
                case (identifier, references) =>
                  tr(
                    td(identifier.toString),
                    td(references.toString),
                  )
              },
            ),
          ),
        )

      section(
        "BuildOutput",
      )(
        dfaToHtml(buildOutput.dfa),
        br,
        expandedGrammarToHtml(buildOutput.expandedGrammar),
        br,
        expandedGrammarToHtml(buildOutput.expandedGrammar2),
      )
    }

    // =====| Usage |=====

    val (mRes, warnings, errors) = aBuildOutput.toTuple

    val htmlFrag =
      html(
        head(
          tag("style")(MyStyles.render),
        ),
        page(
          s"Debug output for: $name",
        )(
          messagesToHtml("Error(s)", errors),
          br,
          messagesToHtml("Warning(s)", warnings),
          br,
          inputToHtml(buildInput),
          br,
          mRes.map(outputToHtml).toList,
        ),
      )

    for {
      _ <- DebugOutputDir.mkdirs.pure[IO]
      outputFile = new File(DebugOutputDir, s"$name.html")
      htmlText = htmlFrag.render
      _ <- IO.writeFile(outputFile, htmlText)
    } yield ()
  }

  /*
  TODO (KR) :

  def main(args: Array[String]): Unit = {

    ???
  }
   */

}
