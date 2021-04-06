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
      name: Marked[Identifier.NonTerminal],
      nt: NonTerminal,
  )

  sealed trait NonTerminal

  sealed trait Element {

    def toNonOpt: (Boolean, NonOptElement) =
      this match {
        case element: NonOptElement =>
          (false, element)
        case Optional(element) =>
          (true, element)
      }

  }

  sealed trait NonOptElement extends Element

  // NonTerminal

  sealed trait StandardNonTerminal extends NonTerminal
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
      with NonOptElement

  object ListNonTerminal {
    sealed trait Type
    object Type {
      case object + extends Type
      case object * extends Type
    }
  }

  final case class AssocNonTerminal(
      assocs: NonEmptyList[(Marked[AssocNonTerminal.Type], Marked[Element])],
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

  sealed trait Identifier extends NonOptElement
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

    def raw(text: String): Identifier.Raw =
      Raw(text)

    def terminal(name: Marked[String]): Attempt[Marked[Identifier.Terminal]] =
      Identifier(name.value) match {
        case terminal: Terminal =>
          name.map(_ => terminal).pure[Attempt]
        case _ =>
          Dead(name.map(n => Msg.userError(s"Invalid terminal: ${n.unesc}")) :: Nil)
      }

    def nonTerminal(name: Marked[String]): Attempt[Marked[Identifier.NonTerminal]] =
      Identifier(name.value) match {
        case nonTerminal: NonTerminal =>
          name.map(_ => nonTerminal).pure[Attempt]
        case _ =>
          Dead(name.map(n => Msg.userError(s"Invalid nonTerminal: ${n.unesc}")) :: Nil)
      }

    private[slyce] def unsafeTerminal(name: Marked[String]): Marked[Identifier.Terminal] =
      name.map(Terminal)

    private[slyce] def unsafeNonTerminal(name: Marked[String]): Marked[Identifier.NonTerminal] =
      name.map(NonTerminal)

  }

  final case class Optional(element: NonOptElement) extends Element

}
