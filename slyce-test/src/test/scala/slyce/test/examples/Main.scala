package slyce.test.examples

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import slyce.generate.building.AugmentedGrammar

object Main {

  def main(args: Array[String]): Unit = {
    Executable
      .fromSubCommands(
        "generate" ->
          Executable.fromSubCommands(
            // "list" -> list.Generate.executable,
          ),
      )(args)
      .runSync
  }

}
