package slyce.generate.main

import klib.Implicits._
import klib.fp.utils.ado
import klib.utils._

import slyce.generate._
import slyce.generate.building._

object Build {

  def buildOutput(buildInput: BuildInput): Attempt[BuildOutput] = {
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
      name = buildInput.name,
      nfa = nfa,
      dfa = dfa,
      expandedGrammar = expandedGrammar,
      deDuplicatedExpandedGrammar = ExpandedGrammar.simplifyAnonLists(expandedGrammar),
    )
  }

  def outputToString(
      `package`: List[String],
      output: BuildOutput,
  ): IndentedString = {
    import IndentedString._

    val headerNote: IndentedString =
      inline(
        "// TODO : ...",
      )

    val packageHeader: IndentedString =
      if (`package`.isEmpty)
        inline()
      else
        inline(
          s"package ${`package`.mkString(".")}",
          Break,
        )

    val imports: IndentedString = // TODO (KR) :
      inline(
        "import slyce.parse._",
      )

    val body: IndentedString = {
      val tokens: IndentedString = // TODO (KR) :
        inline("type Tok = Nothing")

      val raw: IndentedString = // TODO (KR) :
        indented("type Raw = Nothing")

      val parser: IndentedString = {
        val lexer: IndentedString = {

          inline(
            "???, // TODO : Lexer",
          )
        }

        val grammar: IndentedString = {

          inline(
            "???, // TODO : Grammar",
          )
        }

        inline(
          "val parser: Parser[Tok, Raw] =",
          indented(
            "Parser[Tok, Raw](",
            indented(
              lexer,
              grammar,
            ),
            ")",
          ),
        )
      }

      inline(
        s"object ${output.name} {",
        indented(
          Break,
          tokens,
          Break,
          raw,
          Break,
          parser,
          Break,
        ),
        "}",
      )
    }

    inline(
      headerNote,
      Break,
      packageHeader,
      imports,
      Break,
      body,
    )
  }

}
