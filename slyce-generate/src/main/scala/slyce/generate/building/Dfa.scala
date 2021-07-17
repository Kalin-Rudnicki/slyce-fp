package slyce.generate.building

import scala.annotation.tailrec
import scala.collection.immutable.Set

import klib.Implicits._
import klib.fp.types._
import klib.utils._

import slyce.core._
import slyce.generate._
import slyce.generate.input.Lexer

final case class Dfa private (
    modeStarts: Map[String, Dfa.State], // Not actually used, just for reference
    states: NonEmptyList[Dfa.State],
)

object Dfa {

  private def expandEpsilons(states: Set[Nfa.State]): Set[Nfa.State] = {
    val all = findAll(states)(_.epsilonTransitions.map(_.value))
    val filtered = all.filter { state =>
      state.transition.nonEmpty || state.end.nonEmpty
    }

    filtered
  }

  def fromNfa(nfa: Nfa): Attempt[Dfa] = {

    val allNfaStates: Set[Nfa.State] =
      findAll(nfa.modes.toList.map(_._2.value.value).toSet) { state =>
        state.epsilonTransitions.map(_.value) ++ state.transition.map(_._2.value).toOption
      }

    allNfaStates
      .flatMap(_.end.map(_.yields.toMode).toOption)
      .toList
      .flatMap { toMode =>
        toMode.value match {
          case Yields.ToMode.To(name)   => toMode.map(_ => name).someOpt
          case Yields.ToMode.Push(name) => toMode.map(_ => name).someOpt
          case _                        => scala.None
        }
      }
      .map { name =>
        if (nfa.modes.contains(name.value))
          ().pure[Attempt]
        else
          Dead(name.map(name => Msg(s"Invalid mode name: $name")) :: Nil)
      }
      .traverse
      .map { _ =>
        val nfaStatesJoined =
          nfa.modes.toList.map {
            case (k, v) =>
              val expanded = expandEpsilons(Set(v.value.value))
              k -> (expanded, IState.fromNfaStates(expanded))
          }

        val blocked1 = nfaStatesJoined.flatMap(_._2._2._2.toOption)

        val modeMap =
          nfaStatesJoined.map {
            case (k, (nfaStates, (iState, _))) =>
              k -> (nfaStates, iState)
          }.toMap

        val (iStateMap: Map[Set[Nfa.State], IState], blocked2: List[IState.Blocked]) = {
          @tailrec
          def loop(
              seen: Map[Set[Nfa.State], IState],
              unseen: Map[Set[Nfa.State], IState],
              blocked: List[IState.Blocked],
          ): (Map[Set[Nfa.State], IState], List[IState.Blocked]) =
            if (unseen.isEmpty)
              (seen, blocked)
            else {
              val newSeen = seen ++ unseen
              val newUnseen = unseen.flatMap(_._2.children).toSet &~ newSeen.keySet
              val converted = newUnseen.toList.map(s => (s, IState.fromNfaStates(s)))

              loop(
                newSeen,
                converted.map { case (nfaStates, (iState, _)) => (nfaStates, iState) }.toMap,
                converted.flatMap(_._2._2.toOption) ::: blocked,
              )
            }

          loop(
            Map.empty,
            modeMap.map(_._2),
            Nil,
          )
        }

        // TODO (KR) :
        val allBlocked = blocked1 ::: blocked2

        val stateMap: Map[Set[Nfa.State], State] =
          Lazy.selfMap[((Set[Nfa.State], IState), Int), Set[Nfa.State], State](iStateMap.toList.zipWithIndex) {
            case (((nfaStates, iState), i), ef) =>
              nfaStates ->
                State(
                  id = i,
                  transitions = iState.transitions.map {
                    case (k, v) =>
                      k -> v.map(ef)
                  },
                  elseTransition = iState.elseTransition.map(ef),
                  end = iState.end.map(_.map(modeName => ef(modeMap(modeName)._1))),
                )
          }

        val modeStarts: Map[String, State] =
          nfa.modes.map {
            case (k, v) =>
              k -> stateMap(expandEpsilons(Set(v.value.value)))
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
          NonEmptyList(initialState, otherStates),
        )
      }
  }

  private final case class IState private (
      transitions: Map[Set[Char], Maybe[Set[Nfa.State]]],
      elseTransition: Maybe[Set[Nfa.State]],
      end: Maybe[Yields[String]],
  ) {

    def children: Set[Set[Nfa.State]] =
      (elseTransition.toList ::: transitions.toList.flatMap(_._2.toOption)).toSet

  }
  private object IState {

    final case class Blocked(
        yields: Yields[String],
        blocked: NonEmptyList[Lexer.Mode.Line],
    )

    def fromNfaStates(expandedStates: Set[Nfa.State]): (IState, Maybe[Blocked]) = {
      val transitionPairs: List[(InfiniteSet[Char], Nfa.State)] =
        expandedStates.toList.flatMap(_.transition.map(t => (t._1.chars, t._2.value)).toOption)

      val explicitChars: Set[Char] =
        InfiniteSet.explicit[Char](transitionPairs.map(_._1): _*)

      val charTransitions: List[(Char, Maybe[Set[Nfa.State]])] =
        explicitChars.toList
          .map { char =>
            val transitionsTo: Set[Nfa.State] =
              transitionPairs
                .filter(_._1.contains(char))
                .map(_._2)
                .toSet
            char -> expandEpsilons(transitionsTo)
          }
          .map {
            case (char, states) =>
              char -> states.ensure(_.nonEmpty)
          }

      val charMap: Map[Set[Char], Maybe[Set[Nfa.State]]] =
        charTransitions.groupMap(_._2)(_._1).map {
          case (k, v) =>
            v.toSet -> k
        }

      val mElse: Maybe[Set[Nfa.State]] =
        transitionPairs
          .flatMap {
            case (InfiniteSet.Exclusive(_), state) => state.someOpt
            case _                                 => scala.None
          }
          .toSet
          .ensure(_.nonEmpty)
          .map(expandEpsilons)

      val end: Maybe[(Yields[String], Maybe[Blocked])] = {
        val ends: List[Lexer.Mode.Line] = expandedStates.toList.flatMap(_.end.toOption)
        val endsSorted = ends.sortBy(_.priority)
        endsSorted.toNel.map { nel =>
          val yields = nel.head.yields
          (
            yields,
            nel.tail.toNel.map(Blocked(yields, _)),
          )
        }
      }

      (
        IState(
          transitions = charMap,
          elseTransition = mElse,
          end = end.map(_._1),
        ),
        end.flatMap(_._2),
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
