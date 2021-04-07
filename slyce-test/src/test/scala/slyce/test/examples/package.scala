package slyce.test

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import slyce.generate.input._
import slyce.generate.{Main => MainMain}

package object examples {

  def debugExecutable(
      name: String,
      lexer: Lexer,
      grammar: Grammar,
  ): Executable = { (logger: Logger, _) =>
    for {
      _ <- logger() { src =>
        src.ansi.cursorPos(1, 1)
        src.ansi.clearScreen()

        src.info(s"=====| $name |=====")
      }.wrap
      _ <- logger() { src =>
        src.info("Building...")
      }.wrap
      buildInput = MainMain.BuildInput(lexer, grammar)
      aBuildResult <- MainMain.build(buildInput).pure[??]
      _ <- logger() { src =>
        src.info("Writing result...")
      }.wrap
      _ <- MainMain.outputDebug(name, buildInput, aBuildResult).wrap
    } yield ()
  }

}
