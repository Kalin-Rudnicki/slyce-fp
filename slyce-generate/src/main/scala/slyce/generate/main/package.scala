package slyce.generate

import slyce.generate.input._
import slyce.generate.building._

package object main {

  final case class BuildInput(
      lexer: Lexer,
      grammar: Grammar,
  )

  final case class BuildOutput(
      nfa: Nfa,
      dfa: Dfa,
      expandedGrammar: ExpandedGrammar,
      expandedGrammar2: ExpandedGrammar,
  )

}
