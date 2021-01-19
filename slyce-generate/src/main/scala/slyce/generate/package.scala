package slyce

import klib.Implicits._
import klib.fp.types._
import slyce.core._

import scala.annotation.tailrec

package object generate {

  type Attempt[T] = ErrorAccumulator[Marked[Msg], Marked[Msg], T]

  implicit class AttemptOps[T](attempts: List[Attempt[T]]) {

    def traverseErrs: Attempt[List[T]] =
      attempts.traverseErrors

  }

  @tailrec
  def findAll[T](
      unseen: Set[T],
      seen: Set[T] = Set.empty[T],
  )(
      findF: T => Set[T],
  ): Set[T] = {
    val newSeen = seen | unseen
    val newUnseen = unseen.flatMap(findF) &~ newSeen

    if (newUnseen.nonEmpty)
      findAll(newUnseen, newSeen)(findF)
    else
      newSeen
  }

}
