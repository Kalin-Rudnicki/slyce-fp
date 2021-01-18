package slyce

import klib.fp.types._
import slyce.core._

package object generate {

  type Attempt[T] = ErrorAccumulator[Marked[Msg], Marked[Msg], T]

}
