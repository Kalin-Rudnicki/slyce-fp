package slyce.test

import klib.Implicits._
import klib.fp.types._
import klib.utils._, Logger.{helpers => L}, L.Implicits._
import slyce.generate.input._
import slyce.generate.{Main => MainMain}

package object examples {

  def debugExecutable(
      name: String,
      lexer: Lexer,
      grammar: Grammar,
  ): Executable = { (logger: Logger, _) =>
    for {
      _ <- logger(
        L(
          L.ansi.cursorPos(1, 1),
          L.ansi.clearScreen(),
          L.log.important(s"=====| $name |====="),
          L.break(),
          L.log.info("Building..."),
        ),
      ).wrap
      buildInput = MainMain.BuildInput(lexer, grammar)
      aBuildResult <- MainMain.build(buildInput).pure[??]
      _ <- logger(L.log.info("Writing result...")).wrap
      _ <- MainMain.outputDebug(name, buildInput, aBuildResult).wrap
      _ <- logger(L.log.info("Done.")).wrap
    } yield ()
  }

}
