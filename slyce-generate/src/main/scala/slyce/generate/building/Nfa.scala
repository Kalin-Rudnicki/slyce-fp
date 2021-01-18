package slyce.generate.building

import klib.Implicits._
import klib.fp.types._
import klib.utils.Pointer
import slyce.core._
import slyce.generate.Regex.CharClass
import slyce.generate._
import slyce.generate.input.Lexer

final case class Nfa private (
    startMode: String,
    modes: Map[String, Nothing], // TODO (KR) : `Nfa.State` or `_[Nfa.State]`
)

object Nfa {

  def fromLexer(lexer: Lexer): Attempt[Nfa] = {
    val startMode: Attempt[String] =
      lexer.startMode.value
        .aliveIf(lexer.modes.contains)(lexer.startMode.map(m => Msg.userError(s"Invalid start mode: $m")))

    // TODO (KR) :
    ???
  }

  final case class State private (
      transition: Maybe[(CharClass, Pointer[State])],
      epsilonTransitions: Set[Pointer[State]],
      end: Maybe[Lexer.Mode.Line],
  )

  object State {

    def fromMode(mode: Lexer.Mode): Attempt[Pointer[State]] =
      mode.lines
        .map[Attempt[Pointer[State]]] { line =>
          // TODO (KR) :
          ???
        }
        .traverseErrors
        .flatMap[Pointer[State]] { states =>
          // TODO (KR) : Do extra checks

          Pointer(
            State(
              None,
              states.toSet,
              None,
            ),
          ).alive
        }
  }

}
