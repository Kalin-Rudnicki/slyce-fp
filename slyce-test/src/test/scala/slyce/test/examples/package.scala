package slyce.test

import java.io.File

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import klib.utils.Logger.{helpers => L}
import klib.utils.Logger.helpers.Implicits._

import slyce.generate.input._
import slyce.generate.main._

package object examples {

  def debugGenerate(
      name: String,
      lexer: Lexer,
      grammar: Grammar,
  ): Executable = { (logger, args) =>
    val debugOutput = args.contains("-D")
    val fileOutput = args.contains("-F")

    for {
      _ <- logger(
        L(
          L.ansi.cursorPos(1, 1),
          L.ansi.clearScreen(),
          L.log.important(s"=====| $name |====="),
          L.break(),
          L.log.info("Building..."),
        ),
      )
      buildInput = BuildInput(name, lexer, grammar)
      aBuildResult = Build.buildOutput(buildInput)
      _ <-
        if (debugOutput)
          for {
            _ <- logger(L.log.info("Writing DebugOutput..."))
            _ <- OutputDebug.outputDebug(buildInput, aBuildResult)
          } yield ()
        else
          ().pure[IO]
      _ <-
        if (fileOutput)
          aBuildResult match {
            case Alive(buildResult) =>
              for {
                _ <- logger(L.log.info("Writing FileOutput..."))
                pkg = List("slyce", "test", "examples", name)
                outputIdtStr = Build.outputToString(pkg, buildResult)
                outputStr = outputIdtStr.toString("  ")
                outputFile = new File(List(List("slyce-test", "src", "test", "scala"), pkg, List(s"$name.scala")).flatten.mkString("/"))
                _ <- IO.writeFile(outputFile, outputStr)
              } yield ()
            case Dead(_) =>
              ().pure[IO]
          }
        else
          ().pure[IO]
      _ <- logger(L.log.info("Done."))
    } yield ()
  }

}
