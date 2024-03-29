package slyce.generate

import klib.Implicits._
import klib.fp.typeclass._
import klib.fp.types._
import slyce.core._

final case class Yields[I](
    yields: List[Marked[Yields.Yield]],
    toMode: Marked[Yields.ToMode[I]] = Marked(Yields.ToMode.Same),
) {

  def yieldsTerminals: Set[String] =
    yields.flatMap {
      _.value match {
        case Yields.Yield.Terminal(name, _) => name.someOpt
        case _                              => scala.None
      }
    }.toSet

}

object Yields {

  sealed trait Yield {
    val subString: (Maybe[Int], Maybe[Int])
  }
  object Yield {
    final case class Text(
        subString: (Maybe[Int], Maybe[Int]) = (None, None),
    ) extends Yield
    final case class Terminal(
        name: String,
        subString: (Maybe[Int], Maybe[Int]) = (None, None),
    ) extends Yield
    final case class Const(
        text: String,
        subString: (Maybe[Int], Maybe[Int]) = (None, None),
    ) extends Yield
  }

  sealed trait ToMode[+I]
  object ToMode {

    case object Same extends ToMode[Nothing]

    final case class To[I](mode: I) extends ToMode[I]

    final case class Push[I](mode: I) extends ToMode[I]

    // TODO (KR) : Eventual improvement will be allowing for arbitrary number of pops
    //           : eg. Pop(2), Pop(3), Pop(5)
    case object Pop extends ToMode[Nothing]

    implicit val toModeFunctor: Functor[ToMode] =
      new Functor[ToMode] {

        override def map[A, B](t: ToMode[A], f: A => B): ToMode[B] =
          t match {
            case Same =>
              Same
            case To(mode) =>
              To(f(mode))
            case Push(mode) =>
              Push(f(mode))
            case Pop =>
              Pop
          }

      }

  }

  implicit val yieldsFunctor: Functor[Yields] =
    new Functor[Yields] {

      override def map[A, B](t: Yields[A], f: A => B): Yields[B] =
        Yields(
          t.yields,
          t.toMode.map(_.map(f)),
        )

    }

}
