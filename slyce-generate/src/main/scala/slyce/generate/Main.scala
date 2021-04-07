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

    // =====| Helpers |=====

    val TODO = h3("TODO", color := "red")

    // ...

    def pageHeader(text: String): Frag =
      h1(text)

    def sectionHeader(text: String): Frag =
      h2(text)

    def subSectionHeader(text: String): Frag =
      h3(text)

    // ...

    def page(header: String)(_body: Frag*): Frag =
      body(
        pageHeader(header),
        div(
          _body: _*,
        )(
          marginLeft := "25px",
        ),
      )(
        marginTop := "25px",
        marginLeft := "25px",
      )

    def section(header: String)(body: Frag*): Frag =
      div(
        sectionHeader(header),
        div(
          body: _*,
        )(
          marginLeft := "25px",
        ),
      )

    def subSection(header: String)(body: Frag*): Frag =
      div(
        subSectionHeader(header),
        div(
          body: _*,
        )(
          marginLeft := "25px",
        ),
      )

    // =====| Sections |=====

    def messagesToHtml(
        title: String,
        messages: List[Marked[Msg]],
    ): Frag = {

      section(
        s"$title [${messages.size}]:",
      )(
        TODO,
      )
    }

    def inputToHtml(
        buildInput: BuildInput,
    ): Frag = {
      def lexerToHtml(lexer: Lexer): Frag = {

        subSection(
          "Lexer",
        )(
          TODO,
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
