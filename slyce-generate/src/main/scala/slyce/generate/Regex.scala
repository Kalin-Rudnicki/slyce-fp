package slyce.generate

import klib.Implicits._
import klib.fp.types._
import klib.utils.InfiniteSet

import scala.annotation.tailrec

sealed trait Regex {

  def repeat(min: Int, max: Maybe[Int]): Regex =
    Regex.Repeat(this, min, max)

  def maybe: Regex =
    repeat(0, 1.some)

  def exactly(n: Int): Regex =
    repeat(n, n.some)

  def atLeastN(n: Int): Regex =
    repeat(n, None)

  def anyAmount: Regex =
    atLeastN(0)

  def atLeastOnce: Regex =
    atLeastN(1)

}

object Regex {

  final case class CharClass(chars: InfiniteSet[Char]) extends Regex {

    def ~ : CharClass =
      CharClass(this.chars.~)

    def |(that: CharClass): CharClass =
      CharClass(this.chars | that.chars)

    override def toString: String = {
      def buildPairs(chars: Set[Char]): List[Char \/ (Char, Char)] = {
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

      def stringify(name: String, chars: Set[Char]): String = {
        def str(e: Char \/ (Char, Char)): String =
          e match {
            case Left(c) =>
              c.unesc
            case Right((c1, c2)) =>
              s"${c1.unesc}-${c2.unesc}"
          }

        s"$name(${buildPairs(chars).map(str).mkString(", ")})"
      }

      chars match {
        case InfiniteSet.Inclusive(explicit) =>
          stringify("Inclusive", explicit)
        case InfiniteSet.Exclusive(explicit) =>
          stringify("Exclusive", explicit)
      }
    }

  }

  object CharClass {

    // builders

    def union(charClasses: CharClass*): CharClass =
      CharClass(InfiniteSet.union(charClasses.map(_.chars): _*))

    def inclusive(chars: Char*): CharClass =
      CharClass(InfiniteSet.Inclusive(chars.toSet))

    def inclusiveRange(start: Char, end: Char): CharClass =
      inclusive(start.to(end): _*)

    def exclusive(chars: Char*): CharClass =
      CharClass(InfiniteSet.Exclusive(chars.toSet))

    def exclusiveRange(start: Char, end: Char): CharClass =
      exclusive(start.to(end): _*)

    // constants

    val `[A-Z]` : CharClass = inclusiveRange('A', 'Z')
    val `[a-z]` : CharClass = inclusiveRange('a', 'z')
    val `\\d`: CharClass = inclusiveRange('0', '9')
    val `.` : CharClass = exclusive()

    val `[A-Za-z_\\d]` : CharClass = union(`[A-Z]`, `[a-z]`, inclusive('_'), `\\d`)

  }

  final case class Sequence(seq: List[Regex]) extends Regex
  object Sequence {

    def apply(regs: Regex*): Sequence =
      Sequence(regs.toList)

  }

  final case class Group(seqs: NonEmptyList[Sequence]) extends Regex
  object Group {

    def apply(seq0: Sequence, seqN: Sequence*): Group =
      Group(NonEmptyList(seq0, seqN.toList))

  }

  final case class Repeat(reg: Regex, min: Int, max: Maybe[Int]) extends Regex

}
