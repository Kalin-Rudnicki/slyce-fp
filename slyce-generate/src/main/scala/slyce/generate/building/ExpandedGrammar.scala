package slyce.generate.building

import klib.Implicits._
import klib.fp.typeclass.Functor
import klib.fp.types._
import slyce.core._
import slyce.generate._
import slyce.generate.input.Grammar

final case class ExpandedGrammar private (
    startNt: Marked[String], // TODO (KR) : Remove?
    nts: List[ExpandedGrammar.NT],
    aliases: List[ExpandedGrammar.Alias],
)

object ExpandedGrammar {

  type Alias = (Identifier.NonTerminal, Identifier.NonTerminal)

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

      final class Key

      final case class NamedNt(name: String) extends NonTerminal
      final case class ListNt(name: String, `type`: ListType) extends NonTerminal
      final case class AnonListNt(key: Key, `type`: ListType) extends NonTerminal
      final case class AssocNt(name: String, idx: Int) extends NonTerminal
      final case class AnonOptNt(identifier: Identifier) extends NonTerminal
    }

    final case class Terminal(name: String) extends Identifier
    final case class Raw(name: String) extends Identifier

  }

  final case class NT(
      name: Identifier.NonTerminal,
      reductions: NonEmptyList[NT.Reduction],
  )
  object NT {

    def apply(
        name: Identifier.NonTerminal,
        reduction0: Reduction,
        reductionN: Reduction*,
    ): NT =
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
        generatedNts: List[NT],
        aliases: List[Alias],
    )
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
        )
      }

      def combine[W[_]: WToList: Functor, T, T2](expansions: W[Expansion[T]])(cf: W[T] => T2): Expansion[T2] = {
        val expansionList = implicitly[WToList[W]].toList(expansions)

        Expansion(
          cf(expansions.map(_.data)),
          expansionList.flatMap(_.generatedNts),
          expansionList.flatMap(_.aliases),
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
        case snt: Grammar.StandardNonTerminal => expandStandardNonTerminal(Identifier.NonTerminal.NamedNt(name.name), snt)
        case lnt: Grammar.ListNonTerminal     => expandListNonTerminal(name.some, lnt)
        case ant: Grammar.AssocNonTerminal    => expandAssocNonTerminal(name, ant)
      }
    }

    def expandStandardNonTerminal(
        name: Identifier.NonTerminal,
        snt: Grammar.StandardNonTerminal,
    ): Attempt[Expansion[Identifier]] = {

      snt match {
        case Grammar.StandardNonTerminal.`:`(reductions) =>
          for {
            eReductions <- reductions.map(expandList).traverse
            tmpENt = Expansion.combine(eReductions)(ers => NT(name, ers))
          } yield tmpENt
            .copy(generatedNts = tmpENt.data :: tmpENt.generatedNts)
            .map(_ => name)
        case Grammar.StandardNonTerminal.^(reductions) =>
          // TODO (KR) : Need extra information for lifting
          for {
            eReductions <- reductions.map(expandIgnoredList).traverse
            tmpENt = Expansion.combine(eReductions)(ers => NT(name, ers))
          } yield tmpENt
            .copy(generatedNts = tmpENt.data :: tmpENt.generatedNts)
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
              (Identifier.NonTerminal.NamedNt(name.name), id).some,
              id,
            )
          case None =>
            val key = new Identifier.NonTerminal.Key

            (
              None,
              Identifier.NonTerminal.AnonListNt(key, Identifier.NonTerminal.ListType.Simple),
            )
        }
      def createMyIds: (Maybe[Alias], Identifier.NonTerminal, Identifier.NonTerminal) =
        name match {
          case Some(name) =>
            val headId = Identifier.NonTerminal.ListNt(name.name, Identifier.NonTerminal.ListType.Head)

            (
              (Identifier.NonTerminal.NamedNt(name.name), headId).some,
              headId,
              Identifier.NonTerminal.ListNt(name.name, Identifier.NonTerminal.ListType.Tail),
            )
          case None =>
            val key = new Identifier.NonTerminal.Key

            (
              None,
              Identifier.NonTerminal.AnonListNt(key, Identifier.NonTerminal.ListType.Head),
              Identifier.NonTerminal.AnonListNt(key, Identifier.NonTerminal.ListType.Tail),
            )
        }

      (lnt.`type`, lnt.repeat) match {
        case (Grammar.ListNonTerminal.Type.*, None) =>
          val (ma, myId) = createMyId

          for {
            eStart <- expandIgnoredList(lnt.start)
            sR1 = NT.Reduction(eStart.data.elements.appended(myId), eStart.data.liftIdx)
            sNt = NT(myId, sR1, NT.Reduction())
          } yield Expansion(
            myId,
            sNt :: eStart.generatedNts,
            ma.toList ::: eStart.aliases,
          )
        case (Grammar.ListNonTerminal.Type.*, Some(repeat)) =>
          val (ma, myHeadId, myTailId) = createMyIds

          for {
            eStart <- expandIgnoredList(lnt.start)
            eRepeat <- expandIgnoredList(repeat)
            sR1 = NT.Reduction(eStart.data.elements.appended(myTailId), eStart.data.liftIdx)
            rR1 = NT.Reduction(eRepeat.data.elements.appended(myTailId), eRepeat.data.liftIdx)
            sNt = NT(myHeadId, sR1, NT.Reduction())
            rNt = NT(myTailId, rR1, NT.Reduction())
          } yield Expansion(
            myHeadId,
            sNt :: rNt :: eStart.generatedNts ::: eRepeat.generatedNts,
            ma.toList ::: eStart.aliases ::: eRepeat.aliases,
          )
        case (Grammar.ListNonTerminal.Type.+, None) =>
          val (ma, myHeadId, myTailId) = createMyIds

          for {
            eStart <- expandIgnoredList(lnt.start)
            sR1 = NT.Reduction(eStart.data.elements.appended(myTailId), eStart.data.liftIdx)
            sNt = NT(myHeadId, sR1)
            rNt = NT(myTailId, sR1, NT.Reduction())
          } yield Expansion(
            myHeadId,
            sNt :: rNt :: eStart.generatedNts,
            ma.toList ::: eStart.aliases,
          )
        case (Grammar.ListNonTerminal.Type.+, Some(repeat)) =>
          val (ma, myHeadId, myTailId) = createMyIds

          for {
            eStart <- expandIgnoredList(lnt.start)
            eRepeat <- expandIgnoredList(repeat)
            sR1 = NT.Reduction(eStart.data.elements.appended(myTailId), eStart.data.liftIdx)
            rR1 = NT.Reduction(eRepeat.data.elements.appended(myTailId), eRepeat.data.liftIdx)
            sNt = NT(myHeadId, sR1)
            rNt = NT(myTailId, rR1, NT.Reduction())
          } yield Expansion(
            myHeadId,
            sNt :: rNt :: eStart.generatedNts ::: eRepeat.generatedNts,
            ma.toList ::: eStart.aliases ::: eRepeat.aliases,
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
              )
            } yield Expansion.join(
              myExpansion,
              childExpansion,
              opExpansion,
            )
          case Nil =>
            expandStandardNonTerminal(Identifier.NonTerminal.AssocNt(name.name, idx), ant.base)
        }

      for {
        expansion <- rec(
          1,
          ant.assocs.toList,
        )
      } yield expansion
        .copy(aliases = (Identifier.NonTerminal.NamedNt(name.name), Identifier.NonTerminal.AssocNt(name.name, 1)) :: expansion.aliases)
    }

    def expandList(l: List[Marked[Grammar.Element]]): Attempt[Expansion[NT.Reduction]] =
      for {
        expansions <- l.map(expandElement).traverse
      } yield Expansion.combine(expansions)(rs => NT.Reduction(rs))

    def expandIgnoredList(il: IgnoredList[Marked[Grammar.Element]]): Attempt[Expansion[NT.Reduction]] =
      for {
        beforeExpansions <- il.before.map(expandElement).traverse
        unIgnoredExpansion <- expandElement(il.unIgnored)
        afterExpansions <- il.after.map(expandElement).traverse
        expansions = beforeExpansions ::: unIgnoredExpansion :: afterExpansions
      } yield Expansion.combine(expansions)(rs => NT.Reduction(rs, il.before.size.some))

    def expandElement(element: Marked[Grammar.Element]): Attempt[Expansion[Identifier]] = {

      val (isOpt, elem) = element.value.toNonOpt
      val expandedElem = expandNonOptElement(elem)

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
          )
        } yield Expansion.join(optElem, expandedElement)
      else
        expandedElem
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
    )
  }

}
