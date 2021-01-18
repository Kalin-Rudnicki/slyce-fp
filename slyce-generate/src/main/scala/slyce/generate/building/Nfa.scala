package slyce.generate.building

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils.ado
import klib.utils.Pointer
import slyce.core._
import slyce.generate.Regex.CharClass
import slyce.generate._
import slyce.generate.input.Lexer

final case class Nfa private (
    startMode: String,
    modes: Map[String, Marked[Pointer[Nfa.State]]],
)

object Nfa {

  def fromLexer(lexer: Lexer): Attempt[Nfa] = {
    val startMode: Attempt[String] =
      lexer.startMode.value
        .aliveIf(lexer.modes.contains)(lexer.startMode.map(m => Msg.userError(s"Invalid start mode: $m")))

    val modes: Attempt[Map[String, Marked[Pointer[Nfa.State]]]] =
      lexer.modes
        .map(mode => State.fromMode(mode).map(state => (mode.name.value, mode.name.map(_ => state))))
        .traverseErrs
        .map(_.toMap)

    ado[Attempt]
      .join(
        startMode,
        modes,
      )
      .map {
        case (startMode, modes) =>
          Nfa(startMode, modes)
      }
  }

  final case class State private (
      transition: Maybe[(CharClass, Pointer[State])],
      epsilonTransitions: Set[Pointer[State]],
      end: Maybe[Lexer.Mode.Line],
  )

  object State {

    def fromMode(mode: Lexer.Mode): Attempt[Pointer[State]] = {
      def joinStates(states: Pointer[State]*): Pointer[State] =
        Pointer(
          State(
            None,
            states.toSet,
            None,
          ),
        )

      mode.lines
        .map { line =>
          def err(msg: Msg): Marked[Msg] =
            line.regex.map(_ => msg)

          def fromRegex(
              regex: Regex,
              next: Pointer[State],
          ): Attempt[Pointer[State]] =
            regex match {
              case cc @ CharClass(_) =>
                // TODO (KR) : Some sort of check for `Inclusive()` (?)

                Pointer(
                  State(
                    (cc, next).some,
                    Set.empty,
                    None,
                  ),
                ).alive
              case Regex.Sequence(seq) =>
                def rec(
                    reversedQueue: List[Regex],
                    next: Pointer[State],
                ): Attempt[Pointer[State]] =
                  reversedQueue match {
                    case head :: tail =>
                      fromRegex(head, next).flatMap(rec(tail, _))
                    case Nil =>
                      next.alive
                  }

                rec(seq.reverse, next)
              case Regex.Group(seqs) =>
                seqs.toList.map(fromRegex(_, next)).traverseErrs.map(list => joinStates(list: _*))
              case Regex.Repeat(reg, _min, _max) =>
                val min: Attempt[Int] =
                  _min.aliveIf(_ >= 0)(err(Msg.userError(s"min (${_min}) < 0")))

                def repeat(
                    times: Int,
                    next: Pointer[State],
                ): Attempt[Pointer[State]] =
                  if (times > 0)
                    fromRegex(reg, next).flatMap { next =>
                      repeat(times - 1, next)
                    }
                  else
                    next.alive

                _max match {
                  case Some(_max) =>
                    val max: Attempt[Int] =
                      (_max
                        .aliveIf(_ >= 0)(err(Msg.userError(s"max (${_max}) < 0"))): Attempt[Int])
                        .flatMap(_.aliveIf(_ >= _min)(err(Msg.userError(s"max (${_max} < min (${_min}))"))))

                    def doRegexOrSkip(
                        times: Int,
                        next: Pointer[State],
                    ): Attempt[Pointer[State]] =
                      if (times > 0)
                        fromRegex(reg, next).flatMap(doRegexOrSkip(times - 1, _))
                      else
                        next.alive

                    ado[Attempt]
                      .join(
                        min,
                        max,
                      )
                      .flatMap {
                        case (min, max) =>
                          doRegexOrSkip(
                            max - min,
                            next,
                          ).flatMap { next =>
                            repeat(
                              min,
                              next,
                            )
                          }
                      }
                  case None =>
                    /*
                          state -> reg ~> state
                          state ~> next
                     */

                    val loopOnSelf: Attempt[Pointer[State]] =
                      Pointer.withSelfWrapped[State, Attempt] { self =>
                        fromRegex(reg, self).map { state =>
                          Pointer(
                            State(
                              None,
                              Set(
                                next,
                                state,
                              ),
                              None,
                            ),
                          )
                        }
                      }

                    ado[Attempt]
                      .join(
                        min,
                        loopOnSelf,
                      )
                      .flatMap {
                        case (min, loopOnSelf) =>
                          repeat(min, loopOnSelf)
                      }
                }

            }

          fromRegex(
            line.regex.value,
            Pointer(
              State(
                None,
                Set(),
                line.some,
              ),
            ),
          )
        }
        .traverseErrs
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

}
