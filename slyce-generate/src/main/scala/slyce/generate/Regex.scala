package slyce.generate

import klib.Implicits._
import klib.fp.types._
import klib.utils._

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

  def toIdtStr: IndentedString = {
    import IndentedString._
    this match {
      case cc: Regex.CharClass =>
        cc.toString
      case Regex.Sequence(seq) =>
        inline(
          "Sequence",
          indented(
            seq.map(_.toIdtStr),
          ),
        )
      case Regex.Group(seqs) =>
        inline(
          "Group",
          indented(
            seqs.toList.map(_.toIdtStr),
          ),
        )
      case Regex.Repeat(reg, min, max) =>
        inline(
          s"Repeat($min, $max)",
          indented(
            reg.toIdtStr,
          ),
        )
    }
  }

}

object Regex {

  final case class CharClass(chars: InfiniteSet[Char]) extends Regex {

    def ~ : CharClass =
      CharClass(this.chars.~)

    def |(that: CharClass): CharClass =
      CharClass(this.chars | that.chars)

    override def toString: String =
      chars match {
        case InfiniteSet.Inclusive(explicit) =>
          explicit.prettyChars("Inclusive")
        case InfiniteSet.Exclusive(explicit) =>
          explicit.prettyChars("Exclusive")
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

    def apply(str: String): Sequence =
      Sequence(str.map(CharClass.inclusive(_)): _*)

  }

  final case class Group(seqs: NonEmptyList[Sequence]) extends Regex
  object Group {

    def apply(seq0: Sequence, seqN: Sequence*): Group =
      Group(NonEmptyList(seq0, seqN.toList))

  }

  final case class Repeat(reg: Regex, min: Int, max: Maybe[Int]) extends Regex

}
