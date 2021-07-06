package slyce.generate.main

import klib.Implicits._
import klib.fp.utils.ado
import slyce.generate._
import slyce.generate.building._

object Build {

  def build(buildInput: BuildInput): Attempt[BuildOutput] = {
    type LexerItems = (Nfa, Dfa)
    type GrammarItems = (ExpandedGrammar)

    val lexerItems: Attempt[LexerItems] =
      for {
        nfa <- Nfa.fromLexer(buildInput.lexer)
        dfa <- Dfa.fromNfa(nfa)
      } yield (nfa, dfa)

    val grammarItems: Attempt[GrammarItems] =
      for {
        expandedGrammar <- ExpandedGrammar.fromGrammar(buildInput.grammar)
      } yield expandedGrammar

    for {
      joinedItems <- ado[Attempt].join(
        lexerItems,
        grammarItems,
      )
      ((nfa, dfa), expandedGrammar) = joinedItems

      // TODO (KR) : Extra checks
    } yield BuildOutput(
      nfa = nfa,
      dfa = dfa,
      expandedGrammar = expandedGrammar,
      expandedGrammar2 = ExpandedGrammar.simplifyAnonLists(expandedGrammar),
    )
  }

}
