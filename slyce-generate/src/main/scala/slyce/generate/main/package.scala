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
      tokens: Set[String],
      raws: Set[String],
      expandedGrammar: ExpandedGrammar,
      deDuplicatedExpandedGrammar: ExpandedGrammar,
      parsingTable: ParsingTable,
  )

}
