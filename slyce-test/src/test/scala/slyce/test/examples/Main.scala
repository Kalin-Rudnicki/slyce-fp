package slyce.test.examples

import klib.Implicits._
import klib.utils._
import klib.utils.Logger.{helpers => L}

import slyce.core._

object Main {

  val test: Executable = { (logger, _) =>
    for {
      _ <- logger(L.log.info("--- Testing ---"))

      src = Source(0.until(5).map(_ => "1234_6789").mkString("\n"))
      _ <- logger(L.log.info("Source:"))
      _ <- logger(L.log.info(src.input))

      marks = List(
        Marked("Start", Span(Span.Pos.Start, Span.Pos.Start).some),
        Marked("Rest of\n(start)", Span(Span.Pos(2, 1, 2), Span.Pos(4, 1, 4)).some),
        Marked("Should end up as an EOF", Span(Span.Pos(3, 1, 3), Span.Pos(3, 1, 3)).some),
        Marked("(1) 2 messages in same spot", Span(Span.Pos(16, 2, 6), Span.Pos(19, 2, 9)).some),
        Marked("(2) 2 messages in same spot", Span(Span.Pos(16, 2, 6), Span.Pos(19, 2, 9)).some),
        Marked("(3) Actually,\nhere's a 3rd", Span(Span.Pos(16, 2, 6), Span.Pos(19, 2, 9)).some),
        //
        Marked("EOF Message 1"),
        Marked("EOF Message 2"),
        Marked("EOF Message 3\nAnd the second line too"),
      )
      _ <- logger(L.log.info("Source (Marked):"))
      _ <- logger(L.log.info(src.mark(marks)))
    } yield ()
  }

  def main(args: Array[String]): Unit = {
    Executable
      .fromSubCommands(
        "generate" ->
          Executable.fromSubCommands(
            // "list" -> list.Generate.executable,
            "calc" -> calc.Generate.executable,
            "grammar" -> grammar.Generate.executable,
            "tmp" -> tmp.Generate.executable,
          ),
        "test" -> test,
      )(args)
      .runSync
  }

}
