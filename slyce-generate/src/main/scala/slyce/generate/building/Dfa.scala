package slyce.generate.building

import scala.annotation.tailrec

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

  // REMOVE : ...
  import IndentedString._
  import klib.utils.Logger.{helpers => L}
  import klib.utils.Logger.helpers.Implicits._
  private val logger: Logger = Logger(Logger.LogLevel.Debug)
  private implicit class NfaStateSetOps(nfaStates: Set[Nfa.State]) {

    def yieldedTerminals(highlight: Set[String]): List[String] =
      nfaStates
        .flatMap(_.end.toList.flatMap(_.yields.yieldsTerminals))
        .toList
        .sorted
        .map { str =>
          if (highlight.contains(str))
            str.toColorString.green.toString
          else
            str.toColorString.yellow.toString
        }

    def logged(label: String, highlight: Set[String] = Set.empty): Logger.Event =
      L(
        L.log.debug(label),
        L.indented(
          yieldedTerminals(highlight).map(L.log.debug(_)),
        ),
      )

    def loggedIfAny(label: String, highlight: Set[String] = Set.empty): Maybe[Logger.Event] = {
      val yt = yieldedTerminals(highlight)
      yt.nonEmpty.maybe {
        L(
          L.log.debug(label),
          L.indented(
            yieldedTerminals(highlight).map(L.log.debug(_)),
          ),
        )
      }
    }

  }

  def sect(label: String): Unit =
    logger.unsafeLog(
      L(
        L.break(),
        L.log.debug(s"=====| ${label.toColorString.red} |====="),
        L.break(),
      ),
    )

  private def expandEpsilons(states: Set[Nfa.State]): Set[Nfa.State] = {
    val all = findAll(states)(_.epsilonTransitions.map(_.value))
    val filtered = all.filter { state =>
      state.transition.nonEmpty || state.end.nonEmpty
    }

    val classNameStarts =
      List(
        "slyce.",
      )

    val statesL = states.loggedIfAny("[states]", Set("chars"))
    val allL = all.loggedIfAny("[all]", Set("chars"))
    val filteredL = filtered.loggedIfAny("[filtered]", Set("chars"))

    if (statesL.nonEmpty || allL.nonEmpty || filteredL.nonEmpty)
      // REMOVE : ...
      logger.unsafeLog(
        L(
          L.log.debug("expandEpsilons"),
          L.indented(
            L(
              statesL.toList,
              allL.toList,
              filteredL.toList,
              // L.requireFlags("stack-trace")(
              L(
                L.log.debug("[stack-trace]"),
                L.indented(
                  Thread.currentThread.getStackTrace.toList.tail.tail
                    .filter(ste => classNameStarts.exists(ste.getClassName.startsWith(_)))
                    .map(ste => L.log.debug(s"${ste.getFileName.toColorString.cyan}:${ste.getLineNumber.toString.toColorString.magenta}")),
                ),
              ),
              // ),
            ),
          ),
        ),
      )

    filtered
  }

  def fromNfa(nfa: Nfa): Attempt[Dfa] = {

    // REMOVE : ...
    sect("1")

    val allNfaStates: Set[Nfa.State] =
      findAll(nfa.modes.toList.map(_._2.value.value).toSet) { state =>
        state.transition.map(_._2.value).toSet |
          state.epsilonTransitions.map(_.value)
      }

    // REMOVE : ...
    expandEpsilons(allNfaStates)

    // REMOVE : ...
    sect("2")

    allNfaStates
      .flatMap(_.end.map(_.yields.toMode))
      .toList
      .flatMap { toMode =>
        toMode.value match {
          case Yields.ToMode.To(name)   => toMode.map(_ => name).some
          case Yields.ToMode.Push(name) => toMode.map(_ => name).some
          case _                        => None
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
        // REMOVE : ...
        sect("3")

        val nfaStatesJoined =
          nfa.modes.toList.map {
            case (k, v) =>
              val expanded = expandEpsilons(Set(v.value.value))
              k -> (expanded, IState.fromNfaStates(expanded))
          }

        // REMOVE : ...
        sect("4")

        val blocked1 = nfaStatesJoined.flatMap(_._2._2._2)

        val modeMap =
          nfaStatesJoined.map {
            case (k, (nfaStates, (iState, _))) =>
              k -> (nfaStates, iState)
          }.toMap

        // REMOVE : ...
        sect("5")

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
                converted.flatMap(_._2._2) ::: blocked,
              )
            }

          loop(
            Map.empty,
            modeMap.map(_._2),
            Nil,
          )
        }

        // REMOVE : ...
        sect("6")

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

        // REMOVE : ...
        sect("7")

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

        // REMOVE : ...
        sect("8")

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
      (elseTransition.toList ::: transitions.toList.flatMap(_._2)).toSet

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
            case (InfiniteSet.Exclusive(_), state) => state.some
            case _                                 => None
          }
          .toSet
          .ensure(_.nonEmpty)

      val end: Maybe[(Yields[String], Maybe[Blocked])] = {
        val ends: List[Lexer.Mode.Line] = expandedStates.toList.flatMap(_.end)
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
