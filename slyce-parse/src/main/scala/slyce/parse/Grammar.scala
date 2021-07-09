package slyce.parse

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.types._

import slyce.core._

final case class Grammar[Tok, Nt, NtRoot <: Nt](state0: Grammar.State[Tok, Nt, NtRoot]) {

  def buildTree(tokens: List[Tok]): Attempt[NtRoot] = {

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
        case Alive(res) =>
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
        case dead @ Dead(_) =>
          dead
      }

    loop(
      state0,
      Nil,
      tokens.toNel,
    )
  }

}
object Grammar {

  type Stack[Tok, Nt, NtRoot <: Nt] = List[(Tok \/ Nt, State[Tok, Nt, NtRoot])]

  final case class State[Tok, Nt, NtRoot <: Nt](
      handleStack: (Stack[Tok, Nt, NtRoot], Maybe[NonEmptyList[Tok]]) => Attempt[
        (
            State[Tok, Nt, NtRoot],
            Stack[Tok, Nt, NtRoot],
            Maybe[NonEmptyList[Tok]],
        ) \/
          NtRoot,
      ],
      onNt: Nt => Attempt[State[Tok, Nt, NtRoot]],
  )

}
