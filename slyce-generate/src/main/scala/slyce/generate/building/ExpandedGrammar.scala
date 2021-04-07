package slyce.generate.building

import klib.Implicits._
import klib.fp.types._
import slyce.core._
import slyce.generate._
import slyce.generate.input.Grammar

final case class ExpandedGrammar private (
    startNt: Marked[String], // TODO (KR) : Remove?
    nts: List[ExpandedGrammar.NT],
    aliases: ExpandedGrammar.AliasMap,
)

object ExpandedGrammar {

  type AliasMap = Map[ExpandedGrammar.Identifier.NonTerminal, ExpandedGrammar.Identifier.NonTerminal]

  /*
    NOTE (KR) : Types of identifiers
              : `GINT` =  Whatever the Grammar.Identifier.NonTerminal was
              : `UUID` = A sequential int used to differentiate anonymously generated things
              :
              : - Normal -> `GINT`
              : - ListNt -> List[/Head/Tail]__`GINT`
              : - AnonListNt -> AnonList[/Head/Tail]__`UUID`
              : - AnonOptNt -> AnonOpt__`todo`
              : - AssocNt -> Assoc__`GINT`__`idx`

    NOTE (KR) : Problems that need to be addressed:
              : - Would be ideal to have accurate names
              : - Need to avoid having a bunch of duplicate Nts
              :   aka: abc+ | abc+ , and having 2 of the same to represent the
              :   same anonymous lists
              : - When duplicates are de-duplicated, things still need to reference properly

    NOTE (KR) :
   */

  sealed trait Identifier
  object Identifier {
    sealed trait NonTerminal extends Identifier
    object NonTerminal {
      sealed trait ListType
      object ListType {
        case object Simple extends ListType
        case object Head extends ListType
        case object Tail extends ListType
      }

      final class Key

      final case class NormalNt(name: String) extends NonTerminal
      final case class ListNt(name: String, `type`: ListType) extends NonTerminal
      final case class AnonListNt(key: Key, `type`: ListType) extends NonTerminal
      final case class AssocNt(name: String, idx: Int) extends NonTerminal
      // TODO (KR) : Maybe use Grammar.Identifier instead, which already handles differentiating this?
      //           : But other places here dont use it...
      //           : Should they all? Should none of them? Is just 1 ok?
      final case class AnonOptNt(name: String, `type`: AnonOptNt.Type) extends NonTerminal
      object AnonOptNt {
        sealed trait Type
        object Type {
          case object NonTerminal extends Type
          case object Terminal extends Type
          case object Raw extends Type
        }
      }
    }

    final case class Terminal(name: String) extends Identifier
    final case class Raw(name: String) extends Identifier
  }

  final case class NT(
      name: Identifier.NonTerminal,
      reductions: NonEmptyList[NT.Reduction],
  )

  object NT {
    final case class Reduction(elements: List[Identifier])
  }

  // =====|  |=====

  def fromGrammar(grammar: Grammar): Attempt[ExpandedGrammar] = {
    final case class Expansion(
        identifier: Identifier,
        generatedNts: List[NT],
        aliases: AliasMap,
    )

    def expandNonTerminal(nonTerminal: Grammar.NonTerminal): Attempt[Expansion] = {
      // TODO (KR) :
      ???
    }

    def expandElement(element: Marked[Grammar.Element]): Attempt[Expansion] = {

      // TODO (KR) :
      ???
    }

    // TODO (KR) :
    Alive(null)
  }

}
