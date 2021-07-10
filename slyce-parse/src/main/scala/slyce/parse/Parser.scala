package slyce.parse

import klib.Implicits._
import klib.fp.types._

import slyce.core._

final case class Parser[Tok <: Token, Nt, NtRoot <: Nt](
    lexer: Lexer[Tok],
    grammar: Grammar[Tok, Nt, NtRoot],
) {

  def tokenize(source: Source): Attempt[List[Tok]] =
    lexer.tokenize(source)

  def buildTree(tokens: List[Tok]): Attempt[NtRoot] =
    grammar.buildTree(tokens)

  def parse(source: Source): Attempt[NtRoot] =
    for {
      tokens <- tokenize(source)
      raw <- buildTree(tokens)
    } yield raw

  def markTokens(source: Source): String =
    tokenize(source) match {
      case Alive(tokens) =>
        def tokLabel(tok: Tok): String =
          tok.tokName

        source.mark(
          tokens.map { tok =>
            Marked(tokLabel(tok), tok.span.some)
          },
        )
      case Dead(errors) =>
        source.mark(errors)
    }

  def parseAndMarkErrors(source: Source): String \/ NtRoot =
    parse(source) match {
      case Alive(r) =>
        r.right
      case Dead(errors) =>
        source.mark(errors).left
    }

}
