package slyce.core

import klib.fp.typeclass._
import klib.fp.types._

final case class Marked[+T](
    value: T,
    span: Maybe[Span] = None,
)

object Marked {

  implicit val markedFunctor: Functor[Marked] =
    new Functor[Marked] {

      override def map[A, B](t: Marked[A], f: A => B): Marked[B] =
        Marked(f(t.value), t.span)

    }

}
