package slyce.core

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._

final case class Marked[+T](
    value: T,
    span: Span = Span.Unknown,
) {

  def toString(showAbsolute: Boolean): String =
    s"$value @ ${span.toString(showAbsolute)}"

  override def toString: String =
    toString(false)

}

object Marked {

  object Implicits {

    implicit class MarkedIdOps[I](i: I) {

      def marked: Marked[I] =
        Marked(i)

      def marked(span: Span): Marked[I] =
        Marked(i, span)

    }

  }

  implicit val markedMonad: Monad[Marked] =
    new Monad[Marked] {

      override def map[A, B](t: Marked[A], f: A => B): Marked[B] =
        Marked(f(t.value), t.span)

      override def apply[A, B](t: Marked[A], f: Marked[A => B]): Marked[B] =
        Marked(f.value(t.value), Span.joinSpans(t.span, f.span))

      override def pure[A](a: => A): Marked[A] =
        Marked(a)

      override def flatten[A](t: Marked[Marked[A]]): Marked[A] =
        Marked(t.value.value, Span.joinSpans(t.span, t.value.span))

    }

}
