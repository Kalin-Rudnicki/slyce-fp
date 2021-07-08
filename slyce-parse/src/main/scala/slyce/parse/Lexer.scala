package slyce.parse

import scala.annotation.tailrec

import klib.Implicits._
import klib.fp.types._
import slyce.core._

final case class Lexer[Tok](state0: Lexer.State[Tok]) {

  def tokenize(source: Source): Attempt[List[Tok]] = {
    def calcHit(
        hit: Maybe[
          (
              Lexer.Yields[Tok],
              Span.Pos,
              List[Char],
              List[(Char, Span.Pos)],
          ),
        ],
        lastChar: Maybe[(Char, Span.Pos)],
    ): Attempt[(Lexer.Yields.ToMode[Tok], Span.Pos, List[Char], List[Tok])] =
      hit match {
        case Some((yields, pos, chars, rCharAndPos)) =>
          val charAndPos = rCharAndPos.reverse.toArray
          val wholeSpan = Span(charAndPos.head._2, charAndPos.last._2)

          yields.yields.map {
            case Lexer.Yields.Yield(ySpan, build) =>
              def getIdx(i: Int, label: String): Attempt[Int] = {
                val i2 = (i > 0) ? i | (charAndPos.length + i)
                (i2 < 0 || i2 >= charAndPos.length) ?
                  Dead(Marked(s"Substring $label out of bounds [$i2] (length = ${charAndPos.length})", wholeSpan.some) :: Nil) |
                  i2.pure[Attempt]
              }

              for {
                startIdx <- getIdx(ySpan._1.getOrElse(0), "start")
                endIdx <- getIdx(ySpan._2.getOrElse(-1), "end")
                _ <-
                  (startIdx <= endIdx) ?
                    ().pure[Attempt] |
                    Dead(Marked("Substring end before start", wholeSpan.some) :: Nil)
                spanStart = charAndPos(startIdx)._2
                spanEnd = charAndPos(endIdx)._2
                string = charAndPos.slice(startIdx, endIdx + 1).map(_._1).mkString
                tok <- build(string, Span(spanStart, spanEnd))
              } yield tok
          }.traverse match {
            case Alive(newToks) =>
              Alive((yields.toMode, pos, chars, newToks))
            case dead @ Dead(_) =>
              dead
          }
        case None =>
          lastChar match {
            case Some((c, p)) =>
              Dead(Marked(s"Unexpected char ${c.unesc}", Span(p, p).some) :: Nil)
            case None =>
              Dead(Marked("Unexpected EOF") :: Nil)
          }
      }

    // TODO (KR) : It might eventually make sense to de-dup things in order to go faster,
    //           : but for now, I think cleaner is better
    @tailrec
    def loop(
        state: Lexer.State[Tok],
        mode: Lexer.State[Tok],
        modeStack: List[Lexer.State[Tok]],
        pos: Span.Pos,
        chars: List[Char],
        toks: List[Tok],
        seen: List[(Char, Span.Pos)],
        hit: Maybe[(Lexer.Yields[Tok], Span.Pos, List[Char], List[(Char, Span.Pos)])],
    ): Attempt[List[Tok]] =
      (
        chars match {
          case head :: tail =>
            state.on.get(head).toMaybe.flatten.orElse(state.elseOn) match {
              case Some(newState) =>
                val newPos = pos.onChar(head)
                val newSeen = (head, pos) :: seen
                (newState, newPos, newSeen, tail).right
              case None =>
                (head, pos).some.left
            }
          case Nil =>
            None.left
        }
      ) match {
        case Right((newState, newPos, newSeen, tail)) =>
          loop(
            newState,
            mode,
            modeStack,
            newPos,
            tail,
            toks,
            newSeen,
            newState.yields.cata(tm => (tm, newPos, tail, newSeen).some, hit),
          )
        case Left(last) =>
          calcHit(hit, last) match {
            case Alive((toMode, pos, chars, newToks)) =>
              toMode match {
                case Lexer.Yields.ToMode.Same =>
                  loop(
                    mode,
                    mode,
                    modeStack,
                    pos,
                    chars,
                    newToks ::: toks,
                    Nil,
                    None,
                  )
                case Lexer.Yields.ToMode.To(toState) =>
                  loop(
                    toState,
                    toState,
                    modeStack,
                    pos,
                    chars,
                    newToks ::: toks,
                    Nil,
                    None,
                  )
                case Lexer.Yields.ToMode.Push(toState) =>
                  loop(
                    toState,
                    toState,
                    mode :: modeStack,
                    pos,
                    chars,
                    newToks ::: toks,
                    Nil,
                    None,
                  )
                case Lexer.Yields.ToMode.Pop =>
                  modeStack match {
                    case msHead :: msTail =>
                      loop(
                        msHead,
                        msHead,
                        msTail,
                        pos,
                        chars,
                        newToks ::: toks,
                        Nil,
                        None,
                      )
                    case Nil =>
                      Dead(Marked("No state to pop to", Span(pos, pos).some) :: Nil)
                  }
              }
            case dead @ Dead(_) =>
              dead
          }
      }

    loop(
      state0,
      state0,
      Nil,
      Span.Pos.Start,
      source.chars,
      Nil,
      Nil,
      None,
    )
  }

}
object Lexer {

  final case class Yields[Tok](
      yields: List[Yields.Yield[Tok]],
      toMode: Yields.ToMode[Tok],
  )
  object Yields {

    final case class Yield[Tok](
        span: (Maybe[Int], Maybe[Int]),
        build: (String, Span) => Attempt[Tok],
    )

    sealed trait ToMode[+Tok]
    object ToMode {
      case object Same extends ToMode[Nothing]
      final case class To[Tok](state: State[Tok]) extends ToMode[Tok]
      final case class Push[Tok](state: State[Tok]) extends ToMode[Tok]
      case object Pop extends ToMode[Nothing]
    }

  }

  final case class State[Tok](
      id: Int,
      on: Map[Char, Maybe[State[Tok]]],
      elseOn: Maybe[State[Tok]],
      yields: Maybe[Yields[Tok]],
  )

}
