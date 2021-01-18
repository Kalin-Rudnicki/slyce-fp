package slyce.generate

import klib.fp.types.{Maybe, NonEmptyList}
import klib.utils.InfiniteSet

sealed trait Regex {}

object Regex {

  final case class CharClass(chars: InfiniteSet[Char]) extends Regex {

    def ~ : CharClass =
      CharClass(this.chars.~)

    def |(that: CharClass): CharClass =
      CharClass(this.chars | that.chars)

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
