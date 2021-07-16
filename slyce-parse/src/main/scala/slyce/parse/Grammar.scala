package slyce.parse

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.types._

import slyce.core._

final case class Grammar[Tok <: Token, Nt, NtRoot <: Nt](state0: Grammar.State[Tok, Nt, NtRoot]) {

  def buildTree(source: Source, tokens: List[Tok]): Attempt[NtRoot] = {

    @tailrec
    def loop(
        currentState: Grammar.State[Tok, Nt, NtRoot],
        stack: Grammar.Stack[Tok, Nt, NtRoot],
        tokens: Maybe[NonEmptyList[Tok]],
    ): Attempt[NtRoot] =
      currentState.handleStack(
        stack,
        tokens,
      ) match {
        case Some(res) =>
          res match {
            case Left((newCurrentState, newStack, newTokens)) =>
              loop(
                newCurrentState,
                newStack,
                newTokens,
              )
            case Right(ntRoot) =>
              ntRoot.pure[Attempt]
          }
        case None =>
          tokens match {
            case Some(tokens) =>
              Dead(Marked(s"Unexpected token [s${currentState.id}]: ${tokens.head.tokName}", tokens.head.span) :: Nil)
            case None =>
              Dead(Marked(s"Unexpected EOF [s${currentState.id}]", Span.EOF(source)) :: Nil)
          }
      }

    loop(
      state0,
      Nil,
      tokens.toNel,
    )
  }

}
object Grammar {

  type Stack[Tok <: Token, Nt, NtRoot <: Nt] = List[(Tok \/ Nt, State[Tok, Nt, NtRoot])]

  final case class State[Tok <: Token, Nt, NtRoot <: Nt](
      id: Int,
      handleStack: (Stack[Tok, Nt, NtRoot], Maybe[NonEmptyList[Tok]]) => Maybe[
        (
            State[Tok, Nt, NtRoot],
            Stack[Tok, Nt, NtRoot],
            Maybe[NonEmptyList[Tok]],
        ) \/
          NtRoot,
      ],
      onNt: Nt => State[Tok, Nt, NtRoot],
  )

}
