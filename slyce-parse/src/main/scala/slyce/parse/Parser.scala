package slyce.parse

import klib.Implicits._

import slyce.core._

final case class Parser[Tok, Raw](
    lexer: Lexer[Tok],
    grammar: Grammar[Tok, Raw],
) {

  def tokenize(source: Source): Attempt[List[Tok]] =
    lexer.tokenize(source)

  def buildTree(tokens: List[Tok]): Attempt[Raw] =
    grammar.buildTree(tokens)

  def parse(source: Source): Attempt[Raw] =
    for {
      tokens <- tokenize(source)
      raw <- buildTree(tokens)
    } yield raw

}
