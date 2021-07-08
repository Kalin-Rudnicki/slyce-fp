package slyce.parse

import klib.Implicits._

import slyce.core._

final case class Parser[Tok, Nt, NtRoot <: Nt](
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

}
