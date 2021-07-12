package slyce.generate

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._

import scala.annotation.tailrec

final case class IgnoredList[+T](
    before: List[T],
    unIgnored: T,
    after: List[T],
) {

  def unIgnoredIdx: Int =
    before.size

  def toList: List[T] =
    before ::: unIgnored :: after

  def size: Int =
    before.size + after.size + 1

}

object IgnoredList {

  object builder {

    def before[T](ts: T*): Builder1[T] =
      new Builder1(_before = ts.toList)

    def unIgnored[T](t: T): Builder2[T] =
      new Builder2(_before = Nil, _unIgnored = t)

  }

  final class Builder1[+T] private[IgnoredList] (
      _before: List[T],
  ) {

    def unIgnored[T2 >: T](t: T2): Builder2[T2] =
      new Builder2(_before = _before, _unIgnored = t)

  }

  final class Builder2[+T] private[IgnoredList] (
      _before: List[T],
      _unIgnored: T,
  ) {

    def after[T2 >: T](ts: T2*): Builder3[T2] =
      new Builder3(_before = _before, _unIgnored = _unIgnored, _after = ts.toList)

    def build: IgnoredList[T] =
      IgnoredList(
        before = _before,
        unIgnored = _unIgnored,
        after = Nil,
      )

  }

  final class Builder3[+T] private[IgnoredList] (
      _before: List[T],
      _unIgnored: T,
      _after: List[T],
  ) {

    def build: IgnoredList[T] =
      IgnoredList(
        before = _before,
        unIgnored = _unIgnored,
        after = _after,
      )

  }

  // =====|  |=====

  def fromList[T](list: List[(Boolean, T)]): Maybe[IgnoredList[T]] = {
    @tailrec
    def ensure(
        queue: List[(Boolean, T)],
        il: IgnoredList[T],
    ): Maybe[IgnoredList[T]] =
      queue match {
        case (unIgn, _) :: tail =>
          if (unIgn)
            None
          else
            ensure(tail, il)
        case Nil =>
          il.some
      }

    @tailrec
    def find(
        queue: List[(Boolean, T)],
        stack: List[T],
    ): Maybe[IgnoredList[T]] =
      queue match {
        case (unIgn, t) :: tail =>
          if (unIgn)
            ensure(
              tail,
              IgnoredList(stack.reverse, t, tail.map(_._2)),
            )
          else
            find(
              tail,
              t :: stack,
            )
        case Nil =>
          None
      }

    list match {
      case (_, t) :: Nil =>
        IgnoredList(
          before = Nil,
          unIgnored = t,
          after = Nil,
        ).some
      case _ =>
        find(list, Nil)
    }
  }

  implicit val ignoredListFunctor: Functor[IgnoredList] =
    new Functor[IgnoredList] {

      override def map[A, B](t: IgnoredList[A], f: A => B): IgnoredList[B] =
        IgnoredList(
          before = t.before.map(f),
          unIgnored = f(t.unIgnored),
          after = t.after.map(f),
        )

    }

}
