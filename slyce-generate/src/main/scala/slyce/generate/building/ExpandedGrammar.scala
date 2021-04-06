package slyce.generate.building

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.types._
import slyce.core._
import slyce.generate._
import slyce.generate.input.Grammar
import slyce.generate.input.Grammar.StandardNonTerminal

final case class ExpandedGrammar private (
    startNt: Marked[String], // TODO (KR) : Remove?
    // TODO (KR) :
)

object ExpandedGrammar {

  /*
    NOTE (KR) : Types of identifiers
              : - Normal -> `Name`
              : - ListNt -> ListHead__`Name`, ListTail__`Name`
              : - AnonListNt -> AnonListHead__`Num`, AnonListTail__`Num`
              : - AnonOptNt -> AnonOpt__`Name`
              : - AssocNt

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

      final case class NormalNt(name: String) extends NonTerminal
      final case class ListNt(name: String, `type`: ListType) extends NonTerminal
      final case class AnonListNt(id: Int, `type`: ListType) extends NonTerminal
      final case class AssocNt(name: String, idx: Int) extends NonTerminal
    }

    final case class Terminal(name: String, raw: Boolean) extends Identifier
  }

  // =====|  |=====

  def fromGrammar(grammar: Grammar): Attempt[ExpandedGrammar] = {
    def handleNonTerminal(nonTerminal: Grammar.NonTerminal): Nothing =
      nonTerminal match {
        case terminal: StandardNonTerminal =>
          terminal match {
            case StandardNonTerminal.`:`(reductions) =>
              // TODO (KR) :
              ???
            case StandardNonTerminal.^(reductions) =>
              // TODO (KR) :
              ???
          }
        case Grammar.ListNonTerminal(_type, start, repeat) =>
          // TODO (KR) :
          ???
        case Grammar.AssocNonTerminal(assocs, base) =>
          // TODO (KR) :
          ???
      }

    // TODO (KR) :
    ???
  }

}
