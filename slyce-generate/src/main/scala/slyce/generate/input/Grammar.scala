package slyce.generate.input

import klib.Implicits._
import klib.fp.types._
import slyce.core._
import slyce.generate._

final case class Grammar(
    startNt: Marked[String],
)

object Grammar {

  final case class NT(
      name: Marked[String],
  )

  sealed trait Element
  sealed trait NonTerminal

  final case class Identifier(name: Marked[String]) extends Element

}
