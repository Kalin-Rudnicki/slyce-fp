package slyce

import scala.annotation.tailrec
import scala.collection.immutable.Set

import klib.Implicits._
import klib.fp.types._

import slyce.core._

package object generate {

  type Attempt[T] = ErrorAccumulator[Marked[Msg], T]

  implicit class CharSetOps(chars: Set[Char]) {

    def groupChars: List[Char \/ (Char, Char)] = {
      @tailrec
      def loop(
          queue: List[Char],
          stack: List[Char \/ (Char, Char)],
      ): List[Char \/ (Char, Char)] =
        queue match {
          case lower :: tail =>
            @tailrec
            def loop2(
                queue: List[Char],
                current: Char,
            ): (Char, List[Char]) =
              queue match {
                case h :: t =>
                  if (current + 1 == h)
                    loop2(
                      t,
                      h,
                    )
                  else
                    (current, queue)
                case Nil =>
                  (current, Nil)
              }

            val (upper, rest) = loop2(tail, lower)
            val entry =
              if (upper - lower > 0)
                (lower, upper).right
              else
                lower.left
            loop(
              rest,
              entry :: stack,
            )
          case Nil =>
            stack.reverse
        }

      loop(chars.toList.sorted, Nil)
    }

    def prettyChars: String =
      prettyChars("Set")

    def prettyChars(name: String): String = {
      val list =
        chars.groupChars.map {
          case Left(c) =>
            c.unesc
          case Right((c1, c2)) =>
            s"${c1.unesc}-${c2.unesc}"
        }

      s"$name(${list.mkString(", ")})"
    }

  }

  @tailrec
  def findAll[T](
      unseen: Set[T],
      seen: Set[T] = Set.empty[T],
  )(
      findF: T => Set[T],
  ): Set[T] = {
    val newSeen = seen | unseen
    val newUnseen = unseen.flatMap(findF) &~ newSeen

    if (newUnseen.nonEmpty)
      findAll(newUnseen, newSeen)(findF)
    else
      newSeen
  }

}
