package slyce

import klib.fp.types._

package object generate {

  // TODO (KR) :
  type Attempt[T] = ErrorAccumulator[Nothing, Nothing, T]

}
