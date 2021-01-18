package slyce

import klib.Implicits._
import klib.fp.types._
import slyce.core._

package object generate {

  type Attempt[T] = ErrorAccumulator[Marked[Msg], Marked[Msg], T]

  implicit class AttemptOps[T](attempts: List[Attempt[T]]) {

    def traverseErrs: Attempt[List[T]] =
      attempts.traverseErrors

  }

}
