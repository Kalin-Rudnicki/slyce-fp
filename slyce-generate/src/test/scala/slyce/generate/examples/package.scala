package slyce.generate

import klib.Implicits._
import klib.utils._
import slyce.generate._, input._, building._

package object examples {

  def run(
      lexer: Lexer,
      // TODO (KR) :
      //           : grammar: Grammar,
  ): Unit = {

    val logger = Logger(Logger.LogLevel.Debug)

    val nfa: Attempt[Nfa] = Nfa.fromLexer(lexer)

    val dfa: Attempt[Dfa] = nfa.flatMap(Dfa.fromNfa)

    execErrorAccumulator(logger)(dfa) { src => err =>
      src.error(err)
    } { src => warn =>
      src.warning(warn)
    } { src => result =>
      src.info("Modes:")
      src.indented() { src =>
        result.modeStarts.foreach {
          case (k, v) =>
            src.info(s"$k: ${result.stateId(v)}")
        }
      }

      src.info("States:")
      src.indented() { src =>
        result.states.toList.map(_._1).foreach { state =>
          src.info(s"${result.stateId(state)}:")
          src.indented() { src =>
            src.info("Transitions:")
            src.indented() { src =>
              state.transitions.foreach {
                case (k, v) =>
                  src.info(s"${k.prettyChars} => ${v.map(v => result.stateId(v.value))}")
              }
            }
            src.info("ElseTransition:")
            src.indented() { src =>
              state.elseTransition.forEach { v =>
                src.info(result.stateId(v.value))
              }
            }
            src.info("End:")
            src.indented() { src =>
              state.end.forEach { y =>
                src.info(s"Yields (${y.yields.size}): ${y.yields.map(_.value).mkString(", ")}")
                src.info(s"ToMode: ${y.toMode.value.map(v => result.stateId(v.value))}")
              }
            }
          }
        }
      }
    }

  }

}
