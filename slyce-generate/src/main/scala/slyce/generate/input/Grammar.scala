package slyce.generate.input

import klib.Implicits._
import klib.fp.types._
import slyce.core._
import slyce.generate._

final case class Grammar(
    startNt: Marked[String],
    nts: List[Grammar.NT],
)

object Grammar {

  final case class NT(
      name: Marked[String],
      nt: NonTerminal,
  )

  sealed trait NonTerminal
  sealed trait Element

  sealed trait StandardNonTerminal
  object StandardNonTerminal {

    final case class `:`(
        reductions: NonEmptyList[List[Element]],
    ) extends StandardNonTerminal

    final case class ^(
        reductions: NonEmptyList[IgnoredList[Element]],
    ) extends StandardNonTerminal

  }

  final case class ListNonTerminal(
      `type`: ListNonTerminal.Type,
      start: IgnoredList[Element],
      repeat: Maybe[IgnoredList[Element]],
  ) extends NonTerminal
      with Element

  object ListNonTerminal {
    sealed trait Type
    object Type {
      case object + extends Type
      case object * extends Type
    }
  }

  final case class AssocNonTerminal(
      assoc: NonEmptyList[(AssocNonTerminal.Type, Element)],
      base: StandardNonTerminal,
  ) extends NonTerminal

  object AssocNonTerminal {
    sealed trait Type
    object Type {
      case object Left extends Type
      case object Right extends Type
    }
  }

}
