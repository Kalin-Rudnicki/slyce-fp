package slyce.generate

import slyce.generate.input._
import slyce.generate.building._

package object main {

  final case class BuildInput(
      name: String,
      lexer: Lexer,
      grammar: Grammar,
  )

  final case class BuildOutput(
      name: String,
      nfa: Nfa,
      dfa: Dfa,
      tokens: Set[ExpandedGrammar.Identifier.Terminal],
      raws: Set[ExpandedGrammar.Identifier.Raw],
      expandedGrammar: ExpandedGrammar,
      deDuplicatedExpandedGrammar: ExpandedGrammar,
      parsingTable: ParsingTable,
  )

  // TODO (KR) :
  /*
  final case class PartialBuildOutput(
      name: String,
      nfa: Attempt[Nfa],
      dfa: Attempt[Dfa],
      tokens: Attempt[Set[ExpandedGrammar.Identifier.Terminal]],
      raws: Attempt[Set[ExpandedGrammar.Identifier.Raw]],
      expandedGrammar: Attempt[ExpandedGrammar],
      deDuplicatedExpandedGrammar: Attempt[ExpandedGrammar],
      parsingTable: Attempt[ParsingTable],
  )
   */

}
