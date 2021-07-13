package slyce.generate.building

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import slyce.core._
import slyce.generate.Yields.ToMode
import slyce.generate.Yields.Yield
import slyce.generate._
import slyce.generate.input.Lexer

final case class Dfa private (
    modeStarts: Map[String, Dfa.State], // Not actually used, just for reference
    states: NonEmptyList[Dfa.State],
)

object Dfa {

  private def canFreelyTransitionTo(states: Set[Nfa.State]): Set[Nfa.State] = {
    val all = findAll(states)(_.epsilonTransitions.map(_.value))
    val filtered = all.filter { state =>
      state.transition.nonEmpty || state.end.nonEmpty
    }

    // REMOVE : ...
    {
      import IndentedString._

      val statesYields = all.flatMap(_.end.toList.flatMap(_.yields.yieldsTerminals)).toList.sorted
      val allYields = all.flatMap(_.end.toList.flatMap(_.yields.yieldsTerminals)).toList.sorted
      val filteredYields = all.flatMap(_.end.toList.flatMap(_.yields.yieldsTerminals)).toList.sorted

      if (statesYields.nonEmpty || allYields.nonEmpty)
        println {
          inline(
            ">>>",
            indented(
              "states:",
              indented(statesYields),
              "all:",
              indented(allYields),
              "filtered:",
              indented(filteredYields),
            ),
          )
        }
    }

    filtered
  }

  def fromNfa(nfa: Nfa): Attempt[Dfa] = {

    def children(iState: IState): Set[Set[Nfa.State]] =
      iState.transitions.toList.map(_._2).toSet

    val allNfaStates: Set[Nfa.State] =
      findAll(nfa.modes.toList.map(_._2.value.value).toSet) { state =>
        state.transition.map(_._2.value).toSet |
          state.epsilonTransitions.map(_.value)
      }

    val allYields: Set[Yields[String]] =
      allNfaStates.flatMap(_.end.map(_.yields).toOption)

    // REMOVE : ...
    {
      import IndentedString._

      println {
        inline(
          "nfa:",
          indented(
            allNfaStates.toList.flatMap {
              _.end.toList.flatMap(_.yields.yieldsTerminals)
            }.sorted,
          ),
          "allYields:",
          indented(
            allYields
              .flatMap(_.yieldsTerminals)
              .toList
              .sorted,
          ),
        )
      }
    }

    val modeMap: Attempt[(Map[String, (Set[Nfa.State], IState)], List[(Yields[String], Maybe[NonEmptyList[Lexer.Mode.Line]])])] = {
      val nfaStatesJoined =
        nfa.modes.map {
          case (k, v) =>
            val tmp = IState.fromNfaStates(Set(v.value.value))

            k -> tmp
        }

      val blocked1 = nfaStatesJoined.toList.flatMap(_._2._2)

      val map =
        nfaStatesJoined.map {
          case (k, (v1, _, v2)) =>
            k -> (v1, v2)
        }

      val modeTransitions: Map[String, Set[Maybe[Span]]] =
        allYields.toList
          .flatMap { y =>
            val name =
              y.toMode.value match {
                case ToMode.Same =>
                  None
                case ToMode.To(mode) =>
                  mode.some
                case ToMode.Push(mode) =>
                  mode.some
                case ToMode.Pop =>
                  None
              }
            name.map(_ -> y.toMode.span).toOption
          }
          .groupBy(_._1)
          .map {
            case (k, v) =>
              k -> v.map(_._2).toSet
          }

      val missingTransitions: Map[String, Set[Maybe[Span]]] =
        modeTransitions.filterNot(t => nfa.modes.contains(t._1))

      val msgs: List[Marked[Msg]] =
        missingTransitions.toList.flatMap {
          case (mode, spans) =>
            spans.map { span =>
              Marked(Msg.userError(s"Invalid mode: $mode"), span)
            }
        }

      msgs.isEmpty.maybe((map, blocked1)).toEA(msgs: _*)
    }

    modeMap.map {
      case (modeMap, blocked1) =>
        val (iStateMap: Map[Set[Nfa.State], IState], blocked: List[(Yields[String], Maybe[NonEmptyList[Lexer.Mode.Line]])]) = {
          @tailrec
          def loop(
              seen: Map[Set[Nfa.State], IState],
              unseen: Map[Set[Nfa.State], IState],
              blocked: List[(Yields[String], Maybe[NonEmptyList[Lexer.Mode.Line]])],
          ): (Map[Set[Nfa.State], IState], List[(Yields[String], Maybe[NonEmptyList[Lexer.Mode.Line]])]) =
            if (unseen.isEmpty)
              (seen, blocked)
            else {
              val newSeen = seen ++ unseen
              val newUnseen = unseen.toList.flatMap {
                case (_, t) =>
                  children(t)
              }.toSet &~ newSeen.toList.map(_._1).toSet
              val merged = newUnseen.toList.map(IState.fromNfaStates)
              loop(
                newSeen,
                merged.map { case (v1, _, v2) => (v1, v2) }.toMap,
                merged.flatMap(_._2) ::: blocked,
              )
            }

          loop(
            Map.empty,
            modeMap.toList.map(_._2).toMap,
            Nil,
          )
        }

        val stateMap: Map[Set[Nfa.State], State] =
          Lazy.selfMap[((Set[Nfa.State], IState), Int), Set[Nfa.State], State](iStateMap.toList.zipWithIndex) {
            case (((k, v), i), ef) =>
              // REMOVE : ...
              {
                import IndentedString._

                v.elseTransition.foreach { et =>
                  println {
                    inline(
                      "else:",
                      indented(
                        et.toList.flatMap {
                          _.end.toList.flatMap(_.yields.yieldsTerminals)
                        }.sorted,
                      ),
                    )
                  }

                }
              }

              k ->
                State(
                  id = i,
                  transitions = v.transitions.map {
                    case (k, v) =>
                      k -> v.nonEmpty.maybe(ef(v))
                  },
                  elseTransition = v.elseTransition.map(ef),
                  end = v.end.map(_.map(modeName => ef(modeMap(modeName)._1))),
                )
          }

        val modeStarts: Map[String, State] =
          nfa.modes.map {
            case (k, v) =>
              k -> stateMap(canFreelyTransitionTo(Set(v.value.value)))
          }

        val (initialState: State, otherStates: List[State]) = {
          val is = stateMap(modeMap(nfa.startMode)._1)

          (
            is,
            stateMap.toList.map(_._2).filterNot(_ == is),
          )
        }

        // TODO (KR) :
        val allBlocked = blocked1 ::: blocked

        // REMOVE : ...
        {
          import IndentedString._

          println {
            inline(
              "states:",
              indented(
                (initialState :: otherStates).flatMap { state =>
                  state.end.toList.flatMap(_.yieldsTerminals)
                },
              ),
              "picked:",
              indented(
                allBlocked.flatMap(_._1.yieldsTerminals),
              ),
              "blocked:",
              indented(
                allBlocked.map {
                  case (picked, notPicked) =>
                    notPicked.nonEmpty.maybe {
                      inline(
                        picked.toString,
                        indented(
                          notPicked.map(_.toList.map(_.toString)),
                        ),
                      )
                    }
                },
              ),
            )
          }
        }

        Dfa(
          modeStarts,
          NonEmptyList(initialState, otherStates),
        )
    }
  }

  private final case class IState private (
      transitions: Map[Set[Char], Set[Nfa.State]],
      elseTransition: Maybe[Set[Nfa.State]],
      end: Maybe[Yields[String]],
  )

  private object IState {

    def fromNfaStates(rawStates: Set[Nfa.State]): (Set[Nfa.State], Maybe[(Yields[String], Maybe[NonEmptyList[Lexer.Mode.Line]])], IState) = {

      val initialStates: Set[Nfa.State] = canFreelyTransitionTo(rawStates)

      val transitionPairs: List[(InfiniteSet[Char], Nfa.State)] =
        initialStates.toList.flatMap(_.transition.map(t => (t._1.chars, t._2.value)).toOption)

      val explicitChars: Set[Char] =
        InfiniteSet.explicit[Char](transitionPairs.map(_._1): _*)

      val charTransitions: List[(Char, Set[Nfa.State])] =
        explicitChars.toList.map { char =>
          val transitionsTo: Set[Nfa.State] = transitionPairs.filter(_._1.contains(char)).map(_._2).toSet
          char -> canFreelyTransitionTo(transitionsTo)
        }

      val charMap: Map[Set[Char], Set[Nfa.State]] =
        charTransitions.groupBy(_._2).map {
          case (k, v) =>
            v.map(_._1).toSet -> k
        }

      val fromExclusive: Set[Nfa.State] =
        transitionPairs.collect {
          case (InfiniteSet.Exclusive(_), state) =>
            state
        }.toSet

      val end: Maybe[(Yields[String], Maybe[NonEmptyList[Lexer.Mode.Line]])] = {
        val ends: List[Lexer.Mode.Line] = initialStates.toList.flatMap(_.end)
        val endsSorted = ends.sortBy(_.priority)
        endsSorted.toNel.map { nel =>
          (
            nel.head.yields,
            nel.tail.toNel,
          )
        }
      }

      // REMOVE : ...
      {
        import IndentedString._
        println(
          inline(
            ">",
            indented(
              "nfa-states:",
              indented(
                rawStates.toList.flatMap { state =>
                  state.end.toList.flatMap(_.yields.yieldsTerminals)
                },
              ),
            ),
          ),
        )
      }

      (
        initialStates,
        end,
        IState(charMap, fromExclusive.nonEmpty.maybe(fromExclusive), end.map(_._1)),
      )
    }

  }

  final case class State private[Dfa] (
      id: Int,
      transitions: Map[Set[Char], Maybe[Lazy[State]]],
      elseTransition: Maybe[Lazy[State]],
      end: Maybe[Yields[Lazy[State]]],
  )

}
