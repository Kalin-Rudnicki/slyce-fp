package slyce.generate

import klib.Implicits._
import klib.utils._
import slyce.generate._, input._, building._

package object examples {

  def run(
      lexer: Lexer,
      // TODO (KR) :
      //           : grammar: Grammar,
  ): Unit = {

    val logger = Logger(Logger.LogLevel.Debug)

    val nfa: Attempt[Nfa] = Nfa.fromLexer(lexer)

    val dfa: Attempt[Dfa] = nfa.map(Dfa.fromNfa)

    execErrorAccumulator(logger)(dfa) { src => err =>
      src.error(err)
    } { src => warn =>
      src.warning(warn)
    } { src => result => }

  }

}
