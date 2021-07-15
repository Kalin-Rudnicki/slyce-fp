package slyce.parse

import java.io.File

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import klib.utils.Logger.{helpers => L}

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
            Marked(tokLabel(tok), tok.span)
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

  def runTimedParse(logger: Logger, file: File): IO[Unit] = {
    def showTime(label: String)(time: Long): IO[Unit] =
      logger(L.log.info(s"$label: ${Timer.formatFlex(time)}"))

    for {
      sourceText <- IO.readFile(file).timed(showTime("Read file"))
      source = Source(sourceText)
      tokens <- IO(tokenize(source)).timed(showTime("Tokenize"))
      _ <- tokens match {
        case Alive(tokens) =>
          for {
            builtTree <- IO(buildTree(tokens)).timed(showTime("Build tree"))
            _ <- builtTree match {
              case Alive(_) =>
                logger(L.log.info("[DONE]"))
              case Dead(errors) =>
                logger(L.log.fatal(source.mark(errors)))
            }
          } yield ()
        case Dead(errors) =>
          logger(L.log.fatal(source.mark(errors)))
      }
    } yield ()
  }

}
