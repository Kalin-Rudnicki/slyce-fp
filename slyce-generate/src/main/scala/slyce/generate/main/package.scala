package slyce.generate

import klib.fp.Implicits._
import klib.fp.types._
import klib.fp.utils._

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

  final case class PartialBuildOutput(
      name: String,
      nfa: Attempt[Nfa],
      dfa: Attempt[Dfa],
      tokens: Attempt[Set[ExpandedGrammar.Identifier.Terminal]],
      raws: Attempt[Set[ExpandedGrammar.Identifier.Raw]],
      expandedGrammar: Attempt[ExpandedGrammar],
      deDuplicatedExpandedGrammar: Attempt[ExpandedGrammar],
      parsingTable: Attempt[ParsingTable],
  ) {

    def toBuildOutput: Attempt[BuildOutput] =
      ado[Attempt]
        .join(
          nfa,
          dfa,
          tokens,
          raws,
          expandedGrammar,
          deDuplicatedExpandedGrammar,
          parsingTable,
        )
        .map {
          case (nfa, dfa, tokens, raws, expandedGrammar, deDuplicatedExpandedGrammar, parsingTable) =>
            BuildOutput(
              name = name,
              nfa = nfa,
              dfa = dfa,
              tokens = tokens,
              raws = raws,
              expandedGrammar = expandedGrammar,
              deDuplicatedExpandedGrammar = deDuplicatedExpandedGrammar,
              parsingTable = parsingTable,
            )
        }

  }

}
