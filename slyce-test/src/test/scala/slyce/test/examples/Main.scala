package slyce.test.examples

import klib.Implicits._
import klib.utils._

object Main {

  def main(args: Array[String]): Unit = {
    Executable
      .fromSubCommands(
        "generate" ->
          Executable.fromSubCommands(
            // "list" -> list.Generate.executable,
            "calc" -> calc.Generate.executable,
          ),
      )(args)
      .runSync
  }

}
