package slyce.generate.building

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.types._
import klib.fp.utils.ado
import klib.utils._

import slyce.core._
import slyce.generate._

final case class ParsingTable(
    startState: ParsingTable.ParseState,
    states: List[ParsingTable.ParseState],
)
object ParsingTable {

  final case class First(
      terminals: Set[ExpandedGrammar.Identifier.Term],
      epsilon: Boolean,
  )

  final case class Follow(
      terminals: Set[ExpandedGrammar.Identifier.Term],
      end: Boolean,
  ) {
    override def toString: String =
      s"Follow(${(terminals.toList.map(_.toString) ++ end.maybe("$")).mkString(", ")})"
  }

  final case class ParseState(
      terminalActions: Map[Maybe[ExpandedGrammar.Identifier.Term], ParseState.TerminalAction],
      nonTerminalActions: Map[ExpandedGrammar.Identifier.NonTerminal, ParseState.Shift],
      finishesOn: Set[Maybe[ExpandedGrammar.Identifier.Term]],
  )
  object ParseState {

    sealed trait TerminalAction

    final case class Shift(
        to: Lazy[ParseState],
    ) extends TerminalAction

    final case class Reduce(
        produces: ExpandedGrammar.Identifier.NonTerminal,
        rIdentifiers: List[ExpandedGrammar.Identifier],
    ) extends TerminalAction

  }

  // =====|  |=====

  def fromExpandedGrammar(expandedGrammar: ExpandedGrammar): Attempt[ParsingTable] = {
    // TODO (KR) : Possibly improve (?)
    def unaliasNt(nt: ExpandedGrammar.Identifier.NonTerminal): ExpandedGrammar.Identifier.NonTerminal =
      expandedGrammar.aliases.find(_._1 == nt).toMaybe.cata(_._2, nt)

    val firstMap: Map[ExpandedGrammar.Identifier.NonTerminal, First] = {
      final case class Waiting(
          ntName: ExpandedGrammar.Identifier.NonTerminal,
          remaining: List[List[ExpandedGrammar.Identifier]],
          first: Waiting.First,
          madeProgress: Boolean,
      )
      object Waiting {
        final case class First(
            terminals: Set[ExpandedGrammar.Identifier.Term],
            nonTerminals: Set[ExpandedGrammar.Identifier.NonTerminal],
            epsilon: Boolean,
        )
      }

      @tailrec
      def findFirstMap(
          known: Map[ExpandedGrammar.Identifier.NonTerminal, Waiting.First],
          waiting: List[Waiting],
      ): Map[ExpandedGrammar.Identifier.NonTerminal, First] =
        if (waiting.isEmpty) {
          known.map {
            case (k, v) =>
              val joined = findAll(v.nonTerminals)(k => known(unaliasNt(k)).nonTerminals) + k
              val follows = joined.map(k => known(unaliasNt(k)))

              (
                k,
                First(
                  follows.flatMap(_.terminals),
                  follows.exists(_.epsilon),
                ),
              )
          }
        } else if (!waiting.exists(_.madeProgress))
          findFirstMap(
            known ++ waiting.map { w =>
              (
                w.ntName,
                Waiting.First(
                  w.first.terminals,
                  w.first.nonTerminals |
                    w.remaining.flatMap {
                      case (nonTerminal: ExpandedGrammar.Identifier.NonTerminal) :: _ =>
                        nonTerminal.some
                      case _ =>
                        None
                    }.toSet,
                  w.first.epsilon,
                ),
              )
            },
            Nil,
          )
        else {
          val waitingMap: Map[ExpandedGrammar.Identifier.NonTerminal, Waiting] =
            waiting.map(w => (w.ntName, w)).toMap

          @tailrec
          def attemptToAdvance(
              elements: List[ExpandedGrammar.Identifier],
              terminals: Set[ExpandedGrammar.Identifier.Term],
              nonTerminals: Set[ExpandedGrammar.Identifier.NonTerminal],
              madeProgress: Boolean,
          ): (
              Set[ExpandedGrammar.Identifier.Term],
              Set[ExpandedGrammar.Identifier.NonTerminal],
              List[ExpandedGrammar.Identifier],
              Boolean,
          ) \/ Waiting.First =
            elements match {
              case head :: tail =>
                head match {
                  case nonTerminal: ExpandedGrammar.Identifier.NonTerminal =>
                    val unaliasedNt = unaliasNt(nonTerminal)

                    known.get(unaliasedNt).toMaybe match {
                      case Some(follow) =>
                        if (follow.epsilon)
                          attemptToAdvance(
                            tail,
                            terminals,
                            nonTerminals + nonTerminal,
                            true,
                          )
                        else
                          Waiting
                            .First(
                              terminals,
                              nonTerminals + nonTerminal,
                              false,
                            )
                            .right
                      case None =>
                        val ntWaiting = waitingMap(unaliasedNt)

                        if (ntWaiting.first.epsilon)
                          attemptToAdvance(
                            tail,
                            terminals,
                            nonTerminals + nonTerminal,
                            true,
                          )
                        else
                          (
                            terminals,
                            nonTerminals,
                            elements,
                            madeProgress,
                          ).left
                    }
                  case term: ExpandedGrammar.Identifier.Term =>
                    Waiting
                      .First(
                        terminals + term,
                        nonTerminals,
                        false,
                      )
                      .right
                }
              case Nil =>
                Waiting
                  .First(
                    terminals,
                    nonTerminals,
                    true,
                  )
                  .right
            }

          val (notDone, done) =
            waiting.partitionMap { w =>
              val (notDone, done) =
                w.remaining.partitionMap(attemptToAdvance(_, Set.empty, Set.empty, false).toSEither)

              val madeProgress = done.nonEmpty || notDone.exists(_._4)
              val newFollow =
                Waiting.First(
                  w.first.terminals |
                    notDone.flatMap(_._1).toSet |
                    done.flatMap(_.terminals).toSet,
                  w.first.nonTerminals |
                    notDone.flatMap(_._2).toSet |
                    done.flatMap(_.nonTerminals).toSet,
                  w.first.epsilon || done.exists(_.epsilon),
                )

              if (notDone.isEmpty)
                scala.Right((w.ntName, newFollow))
              else
                scala.Left(Waiting(w.ntName, notDone.map(_._3), newFollow, madeProgress))
            }

          findFirstMap(
            known ++ done,
            notDone,
          )
        }

      findFirstMap(
        Map.empty,
        expandedGrammar.nts.map { nt =>
          Waiting(
            nt.name,
            nt.reductions.toList.map(_.elements),
            Waiting.First(Set.empty, Set.empty, false),
            true,
          )
        },
      )
    }

    // ---  ---

    final case class Entry(
        produces: Maybe[ExpandedGrammar.Identifier.NonTerminal],
        rSeen: List[ExpandedGrammar.Identifier],
        unseen: List[ExpandedGrammar.Identifier],
        lookahead: Follow,
    ) {
      lazy val maybeAdvance: Maybe[(ExpandedGrammar.Identifier, Entry)] =
        unseen match {
          case head :: tail =>
            (
              head,
              Entry(
                produces,
                head :: rSeen,
                tail,
                lookahead,
              ),
            ).some
          case Nil =>
            None
        }
    }

    final case class State(
        entries: Set[Entry],
        on: Map[ExpandedGrammar.Identifier, Set[Entry]],
    ) {
      lazy val onToState: Map[ExpandedGrammar.Identifier, State] =
        on.map {
          case (identifier, entries) =>
            (
              identifier,
              expandEntries(entries),
            )
        }
      lazy val finished: List[(Maybe[ExpandedGrammar.Identifier.Term], Entry)] =
        entries.toList.flatMap { e =>
          e.unseen.isEmpty
            .maybe {
              (e.lookahead.terminals.map(_.some) | e.lookahead.end.maybe(None).toSet).toList
                .map((_, e))
            }
            .toList
            .flatten
        }
    }

    lazy val ntMap: Map[ExpandedGrammar.Identifier.NonTerminal, List[List[ExpandedGrammar.Identifier]]] =
      expandedGrammar.nts.map { nt =>
        (
          nt.name,
          nt.reductions.toList.map(_.elements),
        )
      }.toMap

    def findLookahead(
        elements: List[ExpandedGrammar.Identifier],
        follow: Follow,
    ): Follow = {
      @tailrec
      def loop(
          elements: List[ExpandedGrammar.Identifier],
          terminals: Set[ExpandedGrammar.Identifier.Term],
          end: Boolean,
      ): Follow =
        elements match {
          case head :: tail =>
            head match {
              case nonTerminal: ExpandedGrammar.Identifier.NonTerminal =>
                val ntFirst = firstMap(unaliasNt(nonTerminal))
                val newTerminals = ntFirst.terminals | terminals
                if (ntFirst.epsilon)
                  loop(
                    tail,
                    newTerminals,
                    end,
                  )
                else
                  Follow(newTerminals, end)
              case terminal: ExpandedGrammar.Identifier.Term =>
                Follow(terminals + terminal, end)
            }
          case Nil =>
            Follow(follow.terminals | terminals, end | follow.end)
        }

      loop(elements, Set.empty, false)
    }

    def expandEntries(entries: Set[Entry]): State = {
      val allEntries =
        findAll(entries) { e =>
          e.unseen match {
            case head :: tail =>
              head match {
                case nonTerminal: ExpandedGrammar.Identifier.NonTerminal =>
                  val unaliasedNt = unaliasNt(nonTerminal)
                  ntMap(unaliasedNt).map { elements =>
                    Entry(
                      unaliasedNt.some,
                      Nil,
                      elements,
                      findLookahead(tail, e.lookahead),
                    )
                  }.toSet
                case _ =>
                  Set.empty
              }
            case Nil =>
              Set.empty
          }
        }

      // NOTE : This might change if I end up doing CLR(N), maybe not though
      val combinedEntries =
        allEntries
          .groupMap(e => (e.produces, e.rSeen, e.unseen))(_.lookahead)
          .toList
          .map {
            case ((produces, rSeen, unseen), lookaheads) =>
              Entry(
                produces,
                rSeen,
                unseen,
                Follow(
                  lookaheads.flatMap(_.terminals),
                  lookaheads.exists(_.end),
                ),
              )
          }
          .toSet

      State(
        combinedEntries,
        combinedEntries.toList
          .flatMap(_.maybeAdvance)
          .groupMap(_._1)(_._2)
          .map { case (k, v) => (k, v.toSet) },
      )
    }

    val state0: State =
      expandEntries(
        Set(
          Entry(
            None,
            Nil,
            ExpandedGrammar.Identifier.NonTerminal.NamedNt(expandedGrammar.startNt.value) :: Nil,
            Follow(Set.empty, true),
          ),
        ),
      )
    val allStates: Set[State] =
      findAll(Set(state0)) {
        _.onToState.values.toSet
      }

    // ---  ---

    // REMOVE : ...
    {
      import IndentedString._

      println {
        inline(
          allStates.toList
            .sortBy(_.entries.minByOption(_.produces.toString).map(_.produces.toString))
            .zipWithIndex
            .map {
              case (s, i) =>
                inline(
                  s"${i + 1} >",
                  indented(
                    {
                      val pairs =
                        s.entries.toList
                          .sortBy(_.produces.toString)
                          .zipWithIndex
                          .map {
                            case (e, i) =>
                              val idx = (i + 1).toString
                              val produces = e.produces.cata(_.toString, "[RawTree]")
                              val seen = e.rSeen.reverseMap(_.toString).mkString(" ")
                              val unseen = e.unseen.map(_.toString).mkString(" ")
                              val lookahead = e.lookahead.toString
                              (idx, produces, seen, unseen, lookahead)
                          }
                      val idxMax = pairs.maxBy(_._1.length)._1.length
                      val producesMax = pairs.maxBy(_._2.length)._2.length
                      val seenMax = pairs.maxBy(_._3.length)._3.length
                      val unseenMax = pairs.maxBy(_._4.length)._4.length
                      val lookaheadMax = pairs.maxBy(_._5.length)._5.length

                      pairs.map {
                        case (idx, produces, seen, unseen, lookahead) =>
                          s"${idx.padTo(idxMax, ' ')}) ${produces.padTo(producesMax, ' ')} -> ${seen.padTo(seenMax, ' ')} . ${unseen
                            .padTo(unseenMax, ' ')} ; ${lookahead.padTo(lookaheadMax, ' ')}"
                      }
                    },
                  ),
                )
            },
        ).toString("|   ")
      }

      println

      println {
        inline(
          allStates.toList.sortBy(_.entries.minByOption(_.produces.toString).map(_.produces.toString)).map { s =>
            inline(
              ">",
              indented(
                s.finished
                  .map(_._1.toString),
              ),
            )
          },
        ).toString("|    ")
      }
    }

    // ---  ---

    final case class PreParseState(
        terminalActions: Map[Maybe[ExpandedGrammar.Identifier.Term], PreParseState.TerminalAction],
        nonTerminalActions: Map[ExpandedGrammar.Identifier.NonTerminal, PreParseState.Shift],
    )
    object PreParseState {

      sealed trait TerminalAction

      final case class Shift(
          to: State,
      ) extends TerminalAction

      final case class Reduce(
          produces: Maybe[ExpandedGrammar.Identifier.NonTerminal],
          rIdentifiers: List[ExpandedGrammar.Identifier],
      ) extends TerminalAction

    }

    def attemptConvertState(state: State): Attempt[PreParseState] = {
      val (onNt, onT) =
        state.onToState.toList.partitionMap {
          case (identifier, state) =>
            identifier match {
              case nonTerminal: ExpandedGrammar.Identifier.NonTerminal =>
                scala.Left((nonTerminal, PreParseState.Shift(state)))
              case terminal: ExpandedGrammar.Identifier.Term =>
                scala.Right((terminal.some, PreParseState.Shift(state)))
            }
        }
      val finished =
        state.finished
          .map {
            case (lookahead, entry) =>
              (lookahead, PreParseState.Reduce(entry.produces, entry.rSeen))
          }

      val combinedTerminals: List[(Maybe[ExpandedGrammar.Identifier.Term], PreParseState.TerminalAction)] =
        onT ::: finished

      def ensureSingleEntry[A, B](list: List[(A, B)])(onNot1: List[B] => String): Attempt[Map[A, B]] =
        list
          .groupMap(_._1)(_._2)
          .toList
          .map {
            case (_1, _2s) =>
              _2s match {
                case head :: Nil =>
                  (_1, head).pure[Attempt]
                case _ =>
                  Dead(Marked(Msg(onNot1(_2s))) :: Nil)
              }
          }
          .traverse
          .map(_.toMap)

      ado[Attempt]
        .join(
          ensureSingleEntry(combinedTerminals)(_ => "combinedTerminals"), // TODO (KR) : improve message
          ensureSingleEntry(onNt)(_ => "onNt"), // TODO (KR) : improve message
        )
        .map {
          case (terminalActions, nonTerminalActions) =>
            PreParseState(terminalActions, nonTerminalActions)
        }
    }
    val stateMap: Attempt[Map[State, PreParseState]] =
      allStates.toList
        .map(s => attemptConvertState(s).map((s, _)))
        .traverse
        .map(_.toMap)

    stateMap.map { stateMap =>
      val preParseStateMap: Map[PreParseState, ParseState] = {
        lazy val lazyMap: Map[PreParseState, ParseState] =
          stateMap.map {
            case (_, preParseState) =>
              def convertShift(shift: PreParseState.Shift): ParseState.Shift =
                ParseState.Shift(Lazy(lazyMap(stateMap(shift.to))))

              val (terminalActions, finishesOn) =
                preParseState.terminalActions.toList
                  .partitionMap {
                    case (mTerm, action) =>
                      action match {
                        case shift @ PreParseState.Shift(_) =>
                          scala.Left((mTerm, convertShift(shift)))
                        case PreParseState.Reduce(produces, rIdentifiers) =>
                          produces match {
                            case Some(produces) =>
                              scala.Left((mTerm, ParseState.Reduce(produces, rIdentifiers)))
                            case None =>
                              scala.Right(mTerm)
                          }
                      }
                  }

              val nonTerminalActions =
                preParseState.nonTerminalActions.map {
                  case (nonTerminal, shift) =>
                    (
                      nonTerminal,
                      convertShift(shift),
                    )
                }

              (
                preParseState,
                ParseState(
                  terminalActions.toMap,
                  nonTerminalActions,
                  finishesOn.toSet,
                ),
              )
          }

        lazyMap
      }

      ParsingTable(
        preParseStateMap(stateMap(state0)),
        preParseStateMap.values.toList,
      )
    }

  }

}
