package slyce.generate.building

import klib.Implicits._
import klib.fp.types._
import slyce.core._
import slyce.generate._
import slyce.generate.input.Grammar

final case class AugmentedGrammar private (
    startNt: Marked[String], // TODO (KR) : Remove?
    augmentedStart: AugmentedGrammar.ReductionList,
    reductionLists: List[AugmentedGrammar.ReductionList],
    // TODO (KR) : Track `extends`
)

object AugmentedGrammar {

  final case class ReductionList(
      name: Nothing, // TODO (KR) :
      reductions: NonEmptyList[ReductionList.Reduction],
      simplifiers: ReductionList.Simplifiers,
  )

  object ReductionList {

    final case class Reduction(
        idx: Int,
        elements: List[Nothing], // TODO (KR) :
    )

    final case class Simplifiers(
    )
    object Simplifiers {

      // TODO (KR) :

    }

  }

  def fromGrammar(grammar: Grammar): Attempt[AugmentedGrammar] = {

    // TODO (KR) :
    ???
  }

}
