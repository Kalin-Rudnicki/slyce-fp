package slyce

import klib.fp.types._

import slyce.core._

package object parse {

  type Attempt[+R] = ErrorAccumulator[Marked[String], R]

}
