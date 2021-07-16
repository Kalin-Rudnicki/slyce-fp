package slyce.generate.building

import java.util.UUID

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.typeclass.Functor
import klib.fp.types._

import slyce.core._
import slyce.generate._
import slyce.generate.input.Grammar

final case class ExpandedGrammar private (
    startNt: Marked[String], // TODO (KR) : Remove?
    nts: List[ExpandedGrammar.NT[ExpandedGrammar.Identifier.NonTerminal]],
    aliases: List[ExpandedGrammar.Alias],
    extras: List[ExpandedGrammar.ExtraFor],
    withs: List[ExpandedGrammar.With],
)

object ExpandedGrammar {

  val LiftType = "LiftType"
  val Operator = "Operator"
  val Operand = "Operand"

  final case class Alias(
      named: Identifier.NonTerminal,
      actual: Identifier.NonTerminal,
  )
  final case class With(
      identifier: Identifier,
      nt: Identifier.NonTerminal,
      name: String,
  )

  final case class ExtraFor(
      nt: Identifier.NonTerminal,
      extra: Extra,
  )

  sealed trait Extra
  object Extra {

    final case class SimpleToList(
        liftIdx: Int,
        tailIdx: Int,
    ) extends Extra

    final case class HeadTailToList(
        isNel: Boolean,
        headLiftIdx: Int,
        headTailIdx: Int,
        tailNt: Identifier.NonTerminal,
        tailLiftIdx: Int,
        tailTailIdx: Int,
    ) extends Extra

    case object Optional extends Extra

    final case class Lift(
        idxs: NonEmptyList[Int],
    ) extends Extra

    final case class LiftExpr(
        baseName: String,
        assocs: NonEmptyList[Grammar.AssocNonTerminal.Type],
        idxs: NonEmptyList[(Int, Boolean)],
    ) extends Extra

  }

  /*
    NOTE (KR) : Types of identifiers
              : `GINT` =  Whatever the Grammar.Identifier.NonTerminal was
              : `UUID` = A sequential int used to differentiate anonymously generated things
              :
              : - Normal -> `GINT`
              : - ListNt -> List[/Head/Tail]__`GINT`
              : - AnonListNt -> AnonList[/Head/Tail]__`UUID`
              : - AnonOptNt -> AnonOpt__`todo`
              : - AssocNt -> Assoc__`GINT`__`idx`

    NOTE (KR) : Problems that need to be addressed:
              : - Would be ideal to have accurate names
              : - Need to avoid having a bunch of duplicate Nts
              :   aka: abc+ | abc+ , and having 2 of the same to represent the
              :   same anonymous lists
              : - When duplicates are de-duplicated, things still need to reference properly

    NOTE (KR) :
   */

  sealed trait Identifier
  object Identifier {

    def fromGrammarIdentifier(identifier: Grammar.Identifier): Identifier =
      identifier match {
        case Grammar.Identifier.NonTerminal(name) => Identifier.NonTerminal.NamedNt(name)
        case Grammar.Identifier.Terminal(name)    => Identifier.Terminal(name)
        case Grammar.Identifier.Raw(text)         => Identifier.Raw(text)
      }

    sealed trait NonTerminal extends Identifier
    object NonTerminal {
      sealed trait ListType
      object ListType {
        case object Simple extends ListType
        case object Head extends ListType
        case object Tail extends ListType
      }

      final case class NamedNt(name: String) extends NonTerminal
      final case class ListNt(name: String, `type`: ListType) extends NonTerminal
      final case class AnonListNt(key: UUID, `type`: ListType) extends NonTerminal
      final case class AssocNt(name: String, idx: Int) extends NonTerminal
      final case class AnonOptNt(identifier: Identifier) extends NonTerminal
    }

    sealed trait Term extends Identifier
    final case class Terminal(name: String) extends Term
    final case class Raw(name: String) extends Term

  }

  final case class NT[+N <: Identifier.NonTerminal](
      name: N,
      reductions: NonEmptyList[NT.Reduction],
  )
  object NT {

    def apply[N <: Identifier.NonTerminal](
        name: N,
        reduction0: Reduction,
        reductionN: Reduction*,
    ): NT[N] =
      NT(
        name,
        NonEmptyList(reduction0, reductionN.toList),
      )

    final case class Reduction(elements: List[Identifier], liftIdx: Maybe[Int] = None)
    object Reduction {

      def apply(elementN: Identifier*): Reduction =
        Reduction(elementN.toList)

    }

  }

  // =====|  |=====

  def fromGrammar(grammar: Grammar): Attempt[ExpandedGrammar] = {
    final case class Expansion[+T](
        data: T,
        generatedNts: List[NT[Identifier.NonTerminal]],
        aliases: List[Alias],
        withs: List[With],
        extras: List[ExtraFor],
    ) {

      def add(
          generatedNts: List[NT[Identifier.NonTerminal]] = Nil,
          aliases: List[Alias] = Nil,
          withs: List[With] = Nil,
          extras: List[ExtraFor] = Nil,
      ): Expansion[T] =
        Expansion(
          data = this.data,
          generatedNts = generatedNts ::: this.generatedNts,
          aliases = aliases ::: this.aliases,
          withs = withs ::: this.withs,
          extras = extras ::: this.extras,
        )

    }
    object Expansion {

      def join(
          main: Expansion[Identifier],
          extras: Expansion[Identifier]*,
      ): Expansion[Identifier] = {
        val all = main :: extras.toList

        Expansion(
          data = main.data,
          generatedNts = all.flatMap(_.generatedNts),
          aliases = all.flatMap(_.aliases),
          withs = all.flatMap(_.withs),
          extras = all.flatMap(_.extras),
        )
      }

      def combine[W[_]: WToList: Functor, T, T2](expansions: W[Expansion[T]])(cf: W[T] => T2): Expansion[T2] = {
        val expansionList = implicitly[WToList[W]].toList(expansions)

        Expansion(
          cf(expansions.map(_.data)),
          expansionList.flatMap(_.generatedNts),
          expansionList.flatMap(_.aliases),
          expansionList.flatMap(_.withs),
          expansionList.flatMap(_.extras),
        )
      }

      // ...

      trait WToList[W[_]] {

        def toList[T](w: W[T]): List[T]

      }

      implicit val expansionFunctor: Functor[Expansion] =
        new Functor[Expansion] {
          override def map[A, B](t: Expansion[A], f: A => B): Expansion[B] =
            Expansion(
              f(t.data),
              t.generatedNts,
              t.aliases,
              t.withs,
              t.extras,
            )
        }

    }

    implicit val listToList: Expansion.WToList[List] =
      new Expansion.WToList[List] {
        override def toList[T](w: List[T]): List[T] = w
      }

    implicit val nonEmptyListToList: Expansion.WToList[NonEmptyList] =
      new Expansion.WToList[NonEmptyList] {
        override def toList[T](w: NonEmptyList[T]): List[T] = w.toList
      }

    implicit val listFunctor: Functor[List] =
      new Functor[List] {
        override def map[A, B](t: List[A], f: A => B): List[B] = t.map(f)
      }

    def expandNonTerminal(
        name: Grammar.Identifier.NonTerminal,
        nonTerminal: Grammar.NonTerminal,
    ): Attempt[Expansion[Identifier]] = {

      nonTerminal match {
        case snt: Grammar.StandardNonTerminal => expandStandardNonTerminal(Identifier.NonTerminal.NamedNt(name.name), snt, None)
        case lnt: Grammar.ListNonTerminal     => expandListNonTerminal(name.some, lnt)
        case ant: Grammar.AssocNonTerminal    => expandAssocNonTerminal(name, ant)
      }
    }

    def expandStandardNonTerminal(
        name: Identifier.NonTerminal,
        snt: Grammar.StandardNonTerminal,
        exprExtras: Maybe[(String, NonEmptyList[Grammar.AssocNonTerminal.Type])],
    ): Attempt[Expansion[Identifier]] = {

      snt match {
        case Grammar.StandardNonTerminal.`:`(reductions) =>
          val mAddWiths: Maybe[Identifier => With] =
            exprExtras match {
              case Some(_) =>
                // TODO (KR) :
                None
              case None =>
                None
            }
          val lift: Maybe[ExtraFor] =
            exprExtras match {
              case Some(_) =>
                // TODO (KR) :
                None
              case None =>
                None
            }

          for {
            eReductions <- reductions.map(expandList).traverse
            tmpENt = Expansion.combine(eReductions)(ers => NT(name, ers))
          } yield tmpENt
            .copy(generatedNts = tmpENt.data :: tmpENt.generatedNts)
            .map(_ => name)
        case Grammar.StandardNonTerminal.^(reductions) =>
          val mAddWiths: Identifier => Maybe[With] =
            exprExtras match {
              case Some((n, _)) => {
                case Identifier.NonTerminal.NamedNt(n2) if n == n2 => None
                case id                                            => With(id, Identifier.NonTerminal.AssocNt(n, 1), Operand).some
              }
              case None =>
                With(_, name, LiftType).some
            }
          val lift: ExtraFor =
            exprExtras match {
              case Some((n, assocs)) =>
                val base = Identifier.NonTerminal.AssocNt(n, 1)
                ExtraFor(
                  base,
                  Extra.LiftExpr(
                    n,
                    assocs.reverse,
                    reductions.map { r =>
                      (
                        r.unIgnoredIdx,
                        r.unIgnored.value match {
                          case Grammar.Identifier.NonTerminal(n2) => n == n2
                          case _                                  => false
                        },
                      )
                    },
                  ),
                )
              case None =>
                ExtraFor(
                  name,
                  Extra.Lift(reductions.map(_.unIgnoredIdx)),
                )
            }

          // TODO (KR) : Do same sort of self-reference expansion for normal `^` as well?
          for {
            eReductions <- reductions.map(expandIgnoredList(_, mAddWiths.some)).traverse
            tmpENt = Expansion.combine(eReductions)(ers => NT(name, ers))
          } yield tmpENt
            .add(generatedNts = tmpENt.data :: Nil, extras = lift :: Nil)
            .map(_ => name)
      }
    }

    def expandListNonTerminal(
        name: Maybe[Grammar.Identifier.NonTerminal],
        lnt: Grammar.ListNonTerminal,
    ): Attempt[Expansion[Identifier]] = {
      def createMyId: (Maybe[Alias], Identifier.NonTerminal) =
        name match {
          case Some(name) =>
            val id = Identifier.NonTerminal.ListNt(name.name, Identifier.NonTerminal.ListType.Simple)

            (
              Alias(Identifier.NonTerminal.NamedNt(name.name), id).some,
              id,
            )
          case None =>
            (
              None,
              Identifier.NonTerminal.AnonListNt(UUID.randomUUID, Identifier.NonTerminal.ListType.Simple),
            )
        }
      def createMyIds: (Maybe[Alias], Identifier.NonTerminal, Identifier.NonTerminal) =
        name match {
          case Some(name) =>
            val headId = Identifier.NonTerminal.ListNt(name.name, Identifier.NonTerminal.ListType.Head)

            (
              Alias(Identifier.NonTerminal.NamedNt(name.name), headId).some,
              headId,
              Identifier.NonTerminal.ListNt(name.name, Identifier.NonTerminal.ListType.Tail),
            )
          case None =>
            // TODO (KR) : It would be nice to associate the 2 with each other...
            //           : This should be possible by changing all of the de-dupe stuff from using UUID to (UUID, ListType)
            (
              None,
              Identifier.NonTerminal.AnonListNt(UUID.randomUUID, Identifier.NonTerminal.ListType.Head),
              Identifier.NonTerminal.AnonListNt(UUID.randomUUID, Identifier.NonTerminal.ListType.Tail),
            )
        }

      (lnt.`type`, lnt.repeat) match {
        case (Grammar.ListNonTerminal.Type.*, None) =>
          val (ma, myId) = createMyId

          for {
            eStart <- expandIgnoredList(lnt.start, Some(With(_, myId, LiftType).some))
            sR1 = NT.Reduction(eStart.data.elements.appended(myId), eStart.data.liftIdx)
            sNt = NT(myId, sR1, NT.Reduction())
          } yield Expansion(
            myId,
            sNt :: eStart.generatedNts,
            ma.toList ::: eStart.aliases,
            eStart.withs,
            ExtraFor(
              nt = myId,
              extra = Extra.SimpleToList(lnt.start.unIgnoredIdx, lnt.start.size),
            ) ::
              eStart.extras,
          )
        case (Grammar.ListNonTerminal.Type.*, Some(repeat)) =>
          val (ma, myHeadId, myTailId) = createMyIds

          for {
            eStart <- expandIgnoredList(lnt.start, Some(With(_, myHeadId, LiftType).some))
            eRepeat <- expandIgnoredList(repeat, Some(With(_, myHeadId, LiftType).some))
            sR1 = NT.Reduction(eStart.data.elements.appended(myTailId), eStart.data.liftIdx)
            rR1 = NT.Reduction(eRepeat.data.elements.appended(myTailId), eRepeat.data.liftIdx)
            sNt = NT(myHeadId, sR1, NT.Reduction())
            rNt = NT(myTailId, rR1, NT.Reduction())
          } yield Expansion(
            myHeadId,
            sNt :: rNt :: eStart.generatedNts ::: eRepeat.generatedNts,
            ma.toList ::: eStart.aliases ::: eRepeat.aliases,
            eStart.withs ::: eRepeat.withs,
            ExtraFor(
              myHeadId,
              Extra.HeadTailToList(
                isNel = false,
                headLiftIdx = lnt.start.unIgnoredIdx,
                headTailIdx = lnt.start.size,
                tailNt = myTailId,
                tailLiftIdx = repeat.unIgnoredIdx,
                tailTailIdx = repeat.size,
              ),
            ) :: eStart.extras ::: eRepeat.extras,
          )
        case (Grammar.ListNonTerminal.Type.+, None) =>
          val (ma, myHeadId, myTailId) = createMyIds

          for {
            eStart <- expandIgnoredList(lnt.start, Some(With(_, myHeadId, LiftType).some))
            sR1 = NT.Reduction(eStart.data.elements.appended(myTailId), eStart.data.liftIdx)
            sNt = NT(myHeadId, sR1)
            rNt = NT(myTailId, sR1, NT.Reduction())
          } yield Expansion(
            myHeadId,
            sNt :: rNt :: eStart.generatedNts,
            ma.toList ::: eStart.aliases,
            eStart.withs,
            ExtraFor(
              myHeadId,
              Extra.HeadTailToList(
                isNel = true,
                headLiftIdx = lnt.start.unIgnoredIdx,
                headTailIdx = lnt.start.size,
                tailNt = myTailId,
                tailLiftIdx = lnt.start.unIgnoredIdx,
                tailTailIdx = lnt.start.size,
              ),
            ) :: eStart.extras,
          )
        case (Grammar.ListNonTerminal.Type.+, Some(repeat)) =>
          val (ma, myHeadId, myTailId) = createMyIds

          for {
            eStart <- expandIgnoredList(lnt.start, Some(With(_, myHeadId, LiftType).some))
            eRepeat <- expandIgnoredList(repeat, Some(With(_, myHeadId, LiftType).some))
            sR1 = NT.Reduction(eStart.data.elements.appended(myTailId), eStart.data.liftIdx)
            rR1 = NT.Reduction(eRepeat.data.elements.appended(myTailId), eRepeat.data.liftIdx)
            sNt = NT(myHeadId, sR1)
            rNt = NT(myTailId, rR1, NT.Reduction())
          } yield Expansion(
            myHeadId,
            sNt :: rNt :: eStart.generatedNts ::: eRepeat.generatedNts,
            ma.toList ::: eStart.aliases ::: eRepeat.aliases,
            eStart.withs ::: eRepeat.withs,
            ExtraFor(
              myHeadId,
              Extra.HeadTailToList(
                isNel = true,
                headLiftIdx = lnt.start.unIgnoredIdx,
                headTailIdx = lnt.start.size,
                tailNt = myTailId,
                tailLiftIdx = repeat.unIgnoredIdx,
                tailTailIdx = repeat.size,
              ),
            ) :: eStart.extras ::: eRepeat.extras,
          )
      }
    }

    def expandAssocNonTerminal(
        name: Grammar.Identifier.NonTerminal,
        ant: Grammar.AssocNonTerminal,
    ): Attempt[Expansion[Identifier]] = {
      def rec(
          idx: Int,
          queue: List[(Marked[Grammar.AssocNonTerminal.Type], Marked[Grammar.Element])],
      ): Attempt[Expansion[Identifier]] =
        queue match {
          case head :: tail =>
            for {
              childExpansion <- rec(idx + 1, tail)
              opExpansion <- expandElement(head._2)
              myId = Identifier.NonTerminal.AssocNt(name.name, idx)
              myExpansion = Expansion(
                myId,
                NT(
                  myId,
                  head._1.value match {
                    case Grammar.AssocNonTerminal.Type.Left =>
                      NT.Reduction(
                        myId,
                        opExpansion.data,
                        childExpansion.data,
                      )
                    case Grammar.AssocNonTerminal.Type.Right =>
                      NT.Reduction(
                        childExpansion.data,
                        opExpansion.data,
                        myId,
                      )
                  },
                  NT.Reduction(
                    childExpansion.data,
                  ),
                ) :: Nil,
                Nil,
                With(opExpansion.data, Identifier.NonTerminal.AssocNt(name.name, 1), Operator) :: Nil,
                Nil,
              )
            } yield Expansion.join(
              myExpansion,
              childExpansion,
              opExpansion,
            )
          case Nil =>
            expandStandardNonTerminal(
              Identifier.NonTerminal.AssocNt(name.name, idx),
              ant.base,
              (name.name, ant.assocs.map(_._1.value)).some,
            )
        }

      for {
        expansion <- rec(
          1,
          ant.assocs.toList.reverse,
        )
      } yield expansion
        .add(
          aliases = Alias(
            Identifier.NonTerminal.NamedNt(name.name),
            Identifier.NonTerminal.AssocNt(name.name, 1),
          ) :: Nil,
        )

    }

    def expandList(l: List[Marked[Grammar.Element]]): Attempt[Expansion[NT.Reduction]] =
      for {
        expansions <- l.map(expandElement(_)).traverse
      } yield Expansion.combine(expansions)(rs => NT.Reduction(rs))

    def expandIgnoredList(
        il: IgnoredList[Marked[Grammar.Element]],
        mWith: Maybe[Identifier => Maybe[With]] = None,
    ): Attempt[Expansion[NT.Reduction]] =
      for {
        beforeExpansions <- il.before.map(expandElement(_)).traverse
        unIgnoredExpansion <- expandElement(il.unIgnored, mWith)
        afterExpansions <- il.after.map(expandElement(_)).traverse
        expansions = beforeExpansions ::: unIgnoredExpansion :: afterExpansions
      } yield Expansion.combine(expansions)(rs => NT.Reduction(rs, il.before.size.some))

    def expandElement(
        element: Marked[Grammar.Element],
        mWith: Maybe[Identifier => Maybe[With]] = None,
    ): Attempt[Expansion[Identifier]] = {

      val (isOpt, elem) = element.value.toNonOpt
      val expandedElem = expandNonOptElement(elem)

      def addWithIfExists(expansion: Expansion[Identifier]): Expansion[Identifier] =
        mWith match {
          case Some(withF) => expansion.copy(withs = withF(expansion.data).toList ::: expansion.withs)
          case None        => expansion
        }

      if (isOpt)
        for {
          expandedElement <- expandedElem
          optId = Identifier.NonTerminal.AnonOptNt(expandedElement.data)
          optElem = Expansion(
            optId,
            NT(
              optId,
              NT.Reduction(expandedElement.data),
              NT.Reduction(),
            ) :: Nil,
            Nil,
            With(expandedElement.data, optId, LiftType) :: Nil,
            ExtraFor(optId, Extra.Optional) :: Nil,
          )
        } yield Expansion.join(addWithIfExists(optElem), expandedElement)
      else
        for {
          expandedElement <- expandedElem
        } yield addWithIfExists(expandedElement)
    }

    def expandNonOptElement(element: Grammar.NonOptElement): Attempt[Expansion[Identifier]] =
      element match {
        case lnt: Grammar.ListNonTerminal =>
          expandListNonTerminal(None, lnt)
        case identifier: Grammar.Identifier =>
          Expansion(
            Identifier.fromGrammarIdentifier(identifier),
            Nil,
            Nil,
            Nil,
            Nil,
          ).pure[Attempt]
      }

    for {
      expansions <- grammar.nts.map { nt =>
        expandNonTerminal(nt.name.value, nt.nt)
      }.traverse
      combined = Expansion.combine(expansions)(_ => ())
    } yield ExpandedGrammar(
      startNt = grammar.startNt,
      nts = combined.generatedNts,
      aliases = combined.aliases,
      extras = combined.extras,
      withs = combined.withs.distinct,
    )
  }

  // =====|  |=====

  def deDuplicate(expandedGrammar: ExpandedGrammar): ExpandedGrammar = {
    val anonListUUIDMap: Map[UUID, NT[Identifier.NonTerminal.AnonListNt]] =
      expandedGrammar.nts.flatMap { nt =>
        nt.name match {
          case anonList: Identifier.NonTerminal.AnonListNt =>
            (anonList.key, nt.asInstanceOf[NT[Identifier.NonTerminal.AnonListNt]]).someOpt
          case _ =>
            scala.None
        }
      }.toMap

    def getNonBlockedNts(completedUUIDs: Set[UUID]): List[NT[Identifier.NonTerminal.AnonListNt]] = {
      def validAnonList(nt: NT[Identifier.NonTerminal.AnonListNt]): Boolean = {
        def isAlreadyDone: Boolean =
          completedUUIDs.contains(nt.name.key)

        def isBlocked: Boolean =
          nt.reductions.toList.exists { r =>
            r.elements.exists {
              case al: Identifier.NonTerminal.AnonListNt =>
                al.key != nt.name.key && !completedUUIDs.contains(al.key)
              case _ =>
                false
            }
          }

        !(isAlreadyDone || isBlocked)
      }

      anonListUUIDMap.values.toList.filter(validAnonList)
    }

    def mDereferenceNtId(key: UUID, id: Identifier.NonTerminal, found: Map[UUID, UUID]): Maybe[Identifier.NonTerminal] =
      id match {
        case al: Identifier.NonTerminal.AnonListNt =>
          (al.key != key).maybe(dereferenceNtId(id, found))
        case _ =>
          id.some
      }
    def dereferenceNtId(id: Identifier.NonTerminal, found: Map[UUID, UUID]): Identifier.NonTerminal =
      id match {
        case al: Identifier.NonTerminal.AnonListNt =>
          Identifier.NonTerminal
            .AnonListNt(found.getOrElse(al.key, al.key), al.`type`)
            .asInstanceOf[id.type]
        case _ =>
          id
      }

    def mDereferenceId(key: UUID, id: Identifier, found: Map[UUID, UUID]): Maybe[Identifier] =
      id match {
        case terminal: Identifier.NonTerminal =>
          mDereferenceNtId(key, terminal, found)
        case _ =>
          id.some
      }
    def dereferenceId(id: Identifier, found: Map[UUID, UUID]): Identifier =
      id match {
        case terminal: Identifier.NonTerminal =>
          dereferenceNtId(terminal, found)
        case _ =>
          id
      }

    def dereferenceNt(
        nt: NT[Identifier.NonTerminal],
        found: Map[UUID, UUID],
    ): NT[Identifier.NonTerminal] =
      NT(
        dereferenceNtId(nt.name, found),
        nt.reductions.map(r => NT.Reduction(r.elements.map(dereferenceId(_, found)), r.liftIdx)),
      )

    @tailrec
    def findDuplicates(
        found: Map[UUID, UUID],
    ): Map[UUID, UUID] = {
      val completedUUIDs = found.keys.toSet
      val nonBlockedNts = getNonBlockedNts(completedUUIDs)

      if (nonBlockedNts.isEmpty)
        found
      else {
        val nonBlockedDereferenced = nonBlockedNts.map { nt =>
          (
            nt.name.key,
            nt.reductions.map(r => (r.elements.map(mDereferenceId(nt.name.key, _, found)), r.liftIdx)),
          )
        }
        val duplicateLists = nonBlockedDereferenced.groupMap(_._2)(_._1).values.toList
        val newMap = duplicateLists.flatMap(dl => dl.map((_, dl.head))).toMap

        findDuplicates(found ++ newMap)
      }
    }

    def filterRedundantAnonListNts(nts: List[NT[Identifier.NonTerminal]], valid: Set[UUID]): List[NT[Identifier.NonTerminal]] =
      nts.filter { nt =>
        nt.name match {
          case al: Identifier.NonTerminal.AnonListNt =>
            valid.contains(al.key)
          case _ =>
            true
        }
      }

    val duplicateMap = findDuplicates(Map.empty)
    val filteredNts = filterRedundantAnonListNts(expandedGrammar.nts, duplicateMap.values.toSet).map(dereferenceNt(_, duplicateMap))
    val filteredAliases = expandedGrammar.aliases.map(t => Alias(dereferenceNtId(t.named, duplicateMap), dereferenceNtId(t.actual, duplicateMap)))

    def unaliasNt(nt: ExpandedGrammar.Identifier.NonTerminal): ExpandedGrammar.Identifier.NonTerminal =
      filteredAliases.find(_.named == nt).toMaybe.cata(_.actual, nt)

    val deReferenceAliases =
      filteredNts.map { nt =>
        ExpandedGrammar.NT(
          nt.name,
          nt.reductions.map { reduction =>
            ExpandedGrammar.NT.Reduction(
              reduction.elements.map {
                case nt: Identifier.NonTerminal => unaliasNt(nt)
                case i                          => i
              },
              reduction.liftIdx,
            )
          },
        )
      }

    ExpandedGrammar(
      startNt = expandedGrammar.startNt,
      nts = deReferenceAliases.distinct,
      aliases = filteredAliases,
      extras = expandedGrammar.extras.distinct, // TODO (KR) : unalias as well?
      withs = expandedGrammar.withs.distinct, // TODO (KR) : unalias as well?
    )
  }

}
