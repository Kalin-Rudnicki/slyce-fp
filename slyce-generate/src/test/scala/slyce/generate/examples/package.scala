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

    execErrorAccumulator(logger)(nfa) { src => err =>
      src.error(err)
    } { src => warn =>
      src.warning(warn)
    } { src => result =>
      def findAll(
          unseen: Set[Nfa.State],
          seen: Set[Nfa.State],
      ): Set[Nfa.State] = {
        val newSeen = seen | unseen
        val newUnseen = unseen.flatMap { state =>
          state.transition.map(_._2.value).toList.toSet |
            state.epsilonTransitions.map(_.value) &~
              newSeen
        }

        if (newUnseen.nonEmpty)
          findAll(
            newUnseen,
            newSeen,
          )
        else
          newSeen
      }

      val states: Map[Nfa.State, Int] =
        findAll(result.modes.toList.map(_._2.value.value).toSet, Set.empty).toList.zipWithIndex.toMap

      def stateId(state: Nfa.State): String =
        states.get(state).toMaybe.cata(id => s"State#$id", "(?)")

      src.info("Modes:")
      src.indented() { src =>
        result.modes.foreach { mode =>
          src.info(s"${mode._1}: ${stateId(mode._2.value.value)}")
        }
      }

      src.break

      src.info("States:")
      src.indented() { src =>
        states.toList.sortBy(_._2).map(_._1).foreach { state =>
          src.info(s"${stateId(state)}:")
          src.indented() { src =>
            src.info(s"Transition:")
            src.indented() { src =>
              state.transition.forEach(t => src.info(s"${t._1} => ${stateId(t._2.value)}"))
            }
            src.info("EpsilonTransitions:")
            src.indented() { src =>
              state.epsilonTransitions.foreach(t => src.info(stateId(t.value)))
            }
            src.info("End:")
            src.indented() { src =>
              state.end.forEach(src.info(_))
            }
          }
        }
      }
    }

  }

}
