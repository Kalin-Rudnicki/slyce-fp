package slyce.test.examples

import klib.Implicits._
import klib.utils._
import klib.utils.Logger.{helpers => L}

import slyce.core._

object Main {

  def main(args: Array[String]): Unit = {
    Executable
      .fromSubCommands(
        "generate" ->
          Executable.fromSubCommands(
            "grammar" -> grammar.Generate.executable,
            "lexer" -> lexer.Generate.executable,
          ),
      )(args)
      .runSync
  }

}
