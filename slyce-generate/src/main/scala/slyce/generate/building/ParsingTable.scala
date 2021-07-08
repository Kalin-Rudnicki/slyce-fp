package slyce.generate.building

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.types._
import klib.utils._

import slyce.generate._

final case class ParsingTable(
)
object ParsingTable {

  final case class First(
      terminals: Set[ExpandedGrammar.Identifier.Term],
      epsilon: Boolean,
  )

  final case class Follow(
      terminals: Set[ExpandedGrammar.Identifier.Term],
      end: Boolean,
  )

  final case class Production(
      produces: ExpandedGrammar.Identifier.NonTerminal,
      seen: List[ExpandedGrammar.Identifier],
      unseen: List[ExpandedGrammar.Identifier],
      follow: Follow,
  )

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

    final case class Entry(
        produces: Maybe[ExpandedGrammar.Identifier.NonTerminal],
        rSeen: List[ExpandedGrammar.Identifier],
        unseen: List[ExpandedGrammar.Identifier],
        follow: Follow,
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
                follow,
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
    }

    lazy val ntMap: Map[ExpandedGrammar.Identifier.NonTerminal, List[List[ExpandedGrammar.Identifier]]] =
      expandedGrammar.nts.map { nt =>
        (
          nt.name,
          nt.reductions.toList.map(_.elements),
        )
      }.toMap

    def findFollow(
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
            case head :: _ =>
              head match {
                case nonTerminal: ExpandedGrammar.Identifier.NonTerminal =>
                  val unaliasedNt = unaliasNt(nonTerminal)
                  ntMap(unaliasedNt).map { elements =>
                    Entry(
                      unaliasedNt.some,
                      Nil,
                      elements,
                      findFollow(elements, e.follow),
                    )
                  }.toSet
                case _ =>
                  Set.empty
              }
            case Nil =>
              Set.empty
          }
        }

      State(
        allEntries,
        allEntries.toList
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

    {
      import IndentedString._

      println {
        inline(
          allStates.toList.sortBy(_.entries.minByOption(_.produces.toString).map(_.produces.toString)).map { s =>
            inline(
              ">",
              indented(
                s.entries.toList.sortBy(_.produces.toString).map { e =>
                  inline(
                    s"> ${e.produces}",
                    indented(
                      "rSeen:",
                      indented(e.rSeen.map(_.toString)),
                      "unseen:",
                      indented(e.unseen.map(_.toString)),
                      e.follow.toString,
                    ),
                  )
                },
              ),
            )
          },
        ).toString("|   ")
      }
    }

    // TODO (KR) : build table

    ParsingTable(
    ).pure[Attempt]
  }

}
