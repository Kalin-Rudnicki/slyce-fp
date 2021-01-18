package slyce.core

import klib.fp.typeclass._
import klib.fp.types._

final case class Marked[+T](
    value: T,
    span: Maybe[Span] = None,
)

object Marked {

  implicit val markedMonad: Monad[Marked] =
    new Monad[Marked] {

      override def map[A, B](t: Marked[A], f: A => B): Marked[B] =
        Marked(f(t.value), t.span)

      override def apply[A, B](t: Marked[A], f: Marked[A => B]): Marked[B] =
        Marked(f.value(t.value), Span.join(t.span, f.span))

      override def pure[A](a: A): Marked[A] =
        Marked(a)

      override def flatten[A](t: Marked[Marked[A]]): Marked[A] =
        Marked(t.value.value, Span.join(t.span, t.value.span))

    }

}
