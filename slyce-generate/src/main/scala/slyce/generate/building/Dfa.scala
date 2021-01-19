package slyce.generate.building

import scala.annotation.tailrec
import klib.Implicits._
import klib.fp.types._
import klib.utils._
import slyce.core._
import slyce.generate.Yields.ToMode
import slyce.generate._
import slyce.generate.input.Lexer

final case class Dfa private (
    modeStarts: Map[String, Dfa.State], // Not actually used, just for reference
    states: NonEmptyList[(Dfa.State, Int)],
) {

  private val stateMap: Map[Dfa.State, Int] =
    states.toList.toMap

  def stateId(state: Dfa.State): String =
    stateMap.get(state).toMaybe.cata(id => s"State#$id", "(?)")

}

object Dfa {

  private def canFreelyTransitionTo(states: Set[Nfa.State]): Set[Nfa.State] =
    findAll(states)(_.epsilonTransitions.map(_.value)).filter { state =>
      state.transition.nonEmpty | state.end.nonEmpty
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

    val modeMap: Attempt[Map[String, (Set[Nfa.State], IState)]] = {
      val map =
        nfa.modes.map {
          case (k, v) =>
            k -> IState.fromNfaStates(Set(v.value.value))
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

      msgs.isEmpty.maybe(map).toEA(msgs: _*)
    }

    modeMap.map { modeMap =>
      val iStateMap: Map[Set[Nfa.State], IState] = {
        @tailrec
        def loop(
            seen: Map[Set[Nfa.State], IState],
            unseen: Map[Set[Nfa.State], IState],
        ): Map[Set[Nfa.State], IState] =
          if (unseen.isEmpty)
            seen
          else {
            val newSeen = seen ++ unseen
            val newUnseen = unseen.toList.flatMap {
              case (_, t) =>
                children(t)
            }.toSet &~ newSeen.toList.map(_._1).toSet
            loop(
              newSeen,
              newUnseen.toList.map(IState.fromNfaStates).toMap,
            )
          }

        loop(
          Map.empty,
          modeMap.toList.map(_._2).toMap,
        )
      }

      val stateMap: Map[Set[Nfa.State], State] = {
        lazy val lazyMap: Map[Set[Nfa.State], State] =
          iStateMap.map {
            case (k, v) =>
              k ->
                State(
                  transitions = v.transitions.map {
                    case (k, v) =>
                      k -> v.nonEmpty.maybe(Lazy(lazyMap(v)))
                  },
                  elseTransition = v.elseTransition.map(ss => Lazy(lazyMap(ss))),
                  end = v.end.map(_.map(modeName => Lazy(lazyMap(modeMap(modeName)._1)))),
                )
          }

        lazyMap
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

      Dfa(
        modeStarts,
        NonEmptyList(initialState, otherStates).zipWithIndex,
      )
    }
  }

  private final case class IState private (
      transitions: Map[Set[Char], Set[Nfa.State]],
      elseTransition: Maybe[Set[Nfa.State]],
      end: Maybe[Yields[String]],
  )

  private object IState {

    def fromNfaStates(rawStates: Set[Nfa.State]): (Set[Nfa.State], IState) = {

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

      val end: Maybe[Yields[String]] = {
        val ends: List[Lexer.Mode.Line] = initialStates.toList.flatMap(_.end.toOption)
        ends.nonEmpty.maybe(ends.minBy(_.priority).yields)
      }

      initialStates -> IState(charMap, fromExclusive.nonEmpty.maybe(fromExclusive), end)
    }

  }

  final case class State private[Dfa] (
      transitions: Map[Set[Char], Maybe[Lazy[State]]],
      elseTransition: Maybe[Lazy[State]],
      end: Maybe[Yields[Lazy[State]]],
  )

}
