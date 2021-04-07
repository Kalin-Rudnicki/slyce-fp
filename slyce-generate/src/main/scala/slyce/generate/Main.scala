package slyce.generate

import java.io.File

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils.ado
import klib.utils._
import slyce.core._
import slyce.generate._, input._, building._

object Main {

  final case class BuildInput(
      lexer: Lexer,
      grammar: Grammar,
  )

  final case class BuildOutput(
      nfa: Nfa,
      dfa: Dfa,
      expandedGrammar: ExpandedGrammar,
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
    )
  }

  private val DebugOutputDir: File = new File("target/slyce/debug")

  /*
  // TODO (KR) : Delete...
  private type Res = Attempt[(Nfa, Dfa)]
  private def convertRes(logger: Logger, res: Res): ??[Unit] = {
    def convertMarkedMsg(mMsg: Marked[Msg]): Throwable =
      Message(mMsg.toString)

    // TODO (KR) :
    for {
      tmp1 <- res.mapWE(convertMarkedMsg, convertMarkedMsg).wrap[IO]
      (nfa, dfa) = tmp1

      _ <- logger() { src =>
        src.info("Modes:")
        src.indented() { src =>
          dfa.modeStarts.foreach {
            case (k, v) =>
              src.info(s"$k: ${dfa.stateId(v)}")
          }
        }

        src.info("States:")
        src.indented() { src =>
          dfa.states.toList.map(_._1).foreach { state =>
            src.info(s"${dfa.stateId(state)}:")
            src.indented() { src =>
              src.info("Transitions:")
              src.indented() { src =>
                state.transitions.foreach {
                  case (k, v) =>
                    src.info(s"${k.prettyChars} => ${v.map(v => dfa.stateId(v.value))}")
                }
              }
              src.info("ElseTransition:")
              src.indented() { src =>
                state.elseTransition.foreach { v =>
                  src.info(dfa.stateId(v.value))
                }
              }
              src.info("End:")
              src.indented() { src =>
                state.end.foreach { y =>
                  src.info(s"Yields (${y.yields.size}): ${y.yields.map(_.value).mkString(", ")}")
                  src.info(s"ToMode: ${y.toMode.value.map(v => dfa.stateId(v.value))}")
                }
              }
            }
          }
        }
      }.wrap
    } yield ()
  }
   */

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
                  width := "250px",
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

        subSection(
          "Grammar",
        )(
          TODO,
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

      section(
        "BuildOutput",
      )(
        TODO,
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
