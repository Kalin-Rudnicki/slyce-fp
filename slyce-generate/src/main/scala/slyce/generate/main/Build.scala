package slyce.generate.main

import klib.Implicits._
import klib.fp.types._
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
        "import klib.Implicits._",
        "import klib.fp.types._",
        "import slyce.parse._",
      )

    val body: IndentedString = {
      val tokens: IndentedString = // TODO (KR) :
        inline("type Tok = Nothing")

      val raw: IndentedString = // TODO (KR) :
        inline(
          "type Raw = Nothing",
        )

      val parser: IndentedString = {
        val lexer: IndentedString = {
          def makeState(idx: Int, state: slyce.generate.building.Dfa.State): IndentedString = {
            inline(
              s"lazy val s$idx: Lexer.State[Tok] =",
              indented(
                "Lexer.State[Tok](",
                indented(
                  s"$idx,",
                  inline(
                    "Map(",
                    indented(
                      state.transitions.toList
                        .flatMap {
                          case (chars, to) =>
                            val to2 = to.map(t => output.dfa.stateMap(t.value))
                            chars.toList.map((_, to2))
                        }
                        .sortBy(_._1)
                        .map {
                          case (c, to) =>
                            s"${c.toInt}.toChar -> ${to.cata(to => s"s$to.some, /* ${c.unesc} */", "None,")}"
                        },
                    ),
                    "),",
                  ),
                  state.elseTransition.cata(to => s"s${output.dfa.stateMap(to.value)}.some,", "None,"),
                  state.end match {
                    case Some(yields) =>
                      inline(
                        "Lexer.Yields[Tok](",
                        indented(
                          "List(",
                          indented(
                            yields.yields.map { y =>
                              y.value match { // TODO (KR) :
                                case Yields.Yield.Text(subString)           => "???, // TODO : text"
                                case Yields.Yield.Terminal(name, subString) => "???, // TODO : terminal"
                                case Yields.Yield.Const(text)               => "???, // TODO : const"
                              }
                            },
                          ),
                          "),",
                          yields.toMode.value match {
                            case Yields.ToMode.Same       => "Lexer.Yields.ToMode.Same,"
                            case Yields.ToMode.To(mode)   => s"Lexer.Yields.ToMode.To[Tok](s${output.dfa.stateMap(mode.value)}),"
                            case Yields.ToMode.Push(mode) => s"Lexer.Yields.ToMode.Push[Tok](s${output.dfa.stateMap(mode.value)}),"
                            case Yields.ToMode.Pop        => "Lexer.Yields.ToMode.Pop,"
                          },
                        ),
                        ").some,",
                      )
                    case None =>
                      s"None,"
                  },
                ),
                ")",
              ),
              Break,
            )
          }

          inline(
            "Lexer[Tok] {",
            indented(
              output.dfa.states.toList.map {
                case (state, i) =>
                  makeState(i, state)
              },
              "s0",
            ),
            "},",
          )
        }

        val grammar: IndentedString = {

          inline(
            "new Grammar[Tok, Raw] {",
            indented(
              "def buildTree(tokens: List[Tok]): Attempt[Raw] = ??? // TODO : ...",
            ),
            "},",
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
      packageHeader,
      imports,
      Break,
      body,
    )
  }

}
