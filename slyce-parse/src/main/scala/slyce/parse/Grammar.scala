package slyce.parse

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.types._

import slyce.core._

final case class Grammar[Tok <: Token, Nt, NtRoot <: Nt](state0: Grammar.State[Tok, Nt, NtRoot]) {

  def buildTree(tokens: List[Tok]): Attempt[NtRoot] = {

    @tailrec
    def loop(
        currentState: Grammar.State[Tok, Nt, NtRoot],
        stack: Grammar.Stack[Tok, Nt, NtRoot],
        tokens: Maybe[NonEmptyList[Tok]],
    ): Attempt[NtRoot] = {
      {
        // REMOVE : ...
        import klib.utils.IndentedString._

        println {
          inline(
            ">",
            indented(
              s"${"state".toColorString.red}: ${currentState.id}",
              s"${"tokens".toColorString.blue}:",
              indented(
                tokens.map { _.toList.map(_.toString) },
              ),
              s"${"stack".toColorString.green}:",
              indented(
                currentState.id.toString,
                stack.map { s =>
                  val tmp =
                    s._1 match {
                      case Left(tok) => tok.tokName.toColorString.cyan
                      case Right(nt) => nt.getClass.toString.split("NonTerminal")(1).toColorString.magenta
                    }
                  s"$tmp @ ${s._2.id}"
                },
              ),
            ),
          ).toString("    ")
        }
      }

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
