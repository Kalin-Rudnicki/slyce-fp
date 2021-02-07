package slyce.generate.input

import scala.annotation.tailrec

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

  // NonTerminal

  sealed trait StandardNonTerminal
  object StandardNonTerminal {

    final case class `:`(
        reductions: NonEmptyList[List[Marked[Element]]],
    ) extends StandardNonTerminal

    final case class ^(
        reductions: NonEmptyList[IgnoredList[Marked[Element]]],
    ) extends StandardNonTerminal

  }

  final case class ListNonTerminal(
      `type`: ListNonTerminal.Type,
      start: IgnoredList[Marked[Element]],
      repeat: Maybe[IgnoredList[Marked[Element]]],
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
      assoc: NonEmptyList[(Marked[AssocNonTerminal.Type], Marked[Element])],
      base: StandardNonTerminal,
  ) extends NonTerminal

  object AssocNonTerminal {
    sealed trait Type
    object Type {
      case object Left extends Type
      case object Right extends Type
    }
  }

  // Element

  sealed trait Identifier extends Element
  object Identifier {

    final case class Terminal private[Identifier] (name: String) extends Identifier
    final case class NonTerminal private[Identifier] (name: String) extends Identifier
    final case class Raw private[Identifier] (text: String) extends Identifier

    def apply(str: String): Identifier = {
      @tailrec
      def identify(chars: List[Char]): Identifier =
        chars match {
          case Nil =>
            raw(str)
          case c :: rest =>
            if (c == '_')
              identify(rest)
            else if (c.isUpper)
              NonTerminal(str)
            else if (c.isLower)
              Terminal(str)
            else
              raw(str)
        }

      identify(str.toList)
    }

    def raw(text: String): Identifier =
      Raw(text)

    def terminal(name: Marked[String]): Attempt[Identifier] =
      Identifier(name.value) match {
        case terminal: Terminal =>
          terminal.pure[Attempt]
        case _ =>
          Dead(name.map(n => Msg.userError(s"Invalid terminal: ${n.unesc}")) :: Nil)
      }

    def nonTerminal(name: Marked[String]): Attempt[Identifier] =
      Identifier(name.value) match {
        case nonTerminal: NonTerminal =>
          nonTerminal.pure[Attempt]
        case _ =>
          Dead(name.map(n => Msg.userError(s"Invalid nonTerminal: ${n.unesc}")) :: Nil)
      }

  }

}
