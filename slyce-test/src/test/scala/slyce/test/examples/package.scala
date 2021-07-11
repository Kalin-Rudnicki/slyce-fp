package slyce.test

import java.io.File

import org.rogach.scallop._

import klib.Implicits._
import klib.fp.types._
import klib.utils._
import klib.utils.Logger.{helpers => L}
import klib.utils.Logger.helpers.Implicits._

import slyce.core._
import slyce.generate.input._
import slyce.generate.main._
import slyce.parse._

import klib.utils.IndentedString.Break

package object examples {

  def debugGenerate(buildInput: BuildInput): Executable = {
    final class Conf(args: Seq[String]) extends Executable.Conf(args) {
      val debugOutput: ScallopOption[Boolean] = opt()
      val fileOutput: ScallopOption[Boolean] = opt()

      verify()
    }
    object Conf extends Executable.ConfBuilder(new Conf(_))

    Executable.fromConf(Conf) { (logger, conf) =>
      for {
        _ <- logger(
          L(
            L.ansi.cursorPos(1, 1),
            L.ansi.clearScreen(),
            L.log.important(s"=====| ${buildInput.name} |====="),
            L.break(),
            L.log.info("Building..."),
          ),
        )
        aBuildResult = Build.buildOutput(buildInput)
        _ <-
          if (conf.debugOutput())
            for {
              _ <- logger(L.log.info("Writing DebugOutput..."))
              _ <- OutputDebug.outputDebug(buildInput, aBuildResult)
            } yield ()
          else
            ().pure[IO]
        _ <-
          if (conf.fileOutput())
            aBuildResult match {
              case Alive(buildResult) =>
                for {
                  _ <- logger(L.log.info("Writing FileOutput..."))
                  pkg = List("slyce", "test", "examples", buildInput.name)
                  outputIdtStr = Build.outputToString(pkg, buildResult)
                  outputStr = outputIdtStr.toString("  ")
                  outputFile =
                    new File(List(List("slyce-test", "src", "test", "scala"), pkg, List(s"${buildInput.name}.scala")).flatten.mkString("/"))
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

  def debugParse[NtRoot](parser: Parser[_, _, NtRoot])(onTree: (Logger, NtRoot) => IO[Unit]): Executable = {
    val tokenize: Executable = {
      final class Conf(args: Seq[String]) extends Executable.Conf(args) {
        val file: ScallopOption[File] = opt(required = true)
        verify()
      }
      object Conf extends Executable.ConfBuilder(new Conf(_))

      Executable.fromConf(Conf) { (logger, conf) =>
        for {
          _ <- logger(
            L(
              L.ansi.cursorPos(1, 1),
              L.ansi.clearScreen(),
              L.log.info(s"Parsing: ${conf.file()}"),
              L.break(),
            ),
          )

          sourceText <- IO.readFile(conf.file())
          _ <- logger(
            L(
              L.log.info(sourceText),
              L.break(),
            ),
          )
          source = Source(sourceText)

          markedSource = parser.markTokens(source)
          _ <- logger(L.log.info(markedSource))
        } yield ()
      }
    }

    val tree: Executable = {
      final class Conf(args: Seq[String]) extends Executable.Conf(args) {
        val file: ScallopOption[File] = opt(required = true)
        verify()
      }
      object Conf extends Executable.ConfBuilder(new Conf(_))

      Executable.fromConf(Conf) { (logger, conf) =>
        for {
          _ <- logger(
            L(
              L.ansi.cursorPos(1, 1),
              L.ansi.clearScreen(),
              L.log.info(s"Building tree: ${conf.file()}"),
              L.break(),
            ),
          )

          sourceText <- IO.readFile(conf.file())
          source = Source(sourceText)
          /*
          _ <- logger(
            L(
              L.log.info(sourceText),
              L.break(),
            ),
          )
           */

          _ <- parser.parseAndMarkErrors(source) match {
            case Right(root) =>
              onTree(logger, root)
            case Left(markedSource) =>
              logger(L.log.info(markedSource))
          }
        } yield ()
      }
    }

    val time: Executable = {
      final class Conf(args: Seq[String]) extends Executable.Conf(args) {
        val file: ScallopOption[File] = opt(required = true)
        verify()
      }
      object Conf extends Executable.ConfBuilder(new Conf(_))

      Executable.fromConf(Conf) { (logger, conf) =>
        for {
          _ <- logger(
            L(
              L.ansi.cursorPos(1, 1),
              L.ansi.clearScreen(),
              L.log.info(s"Showing time: ${conf.file()}"),
              L.break(),
            ),
          )

          _ <- parser.runTimedParse(logger, conf.file())
        } yield ()
      }
    }

    Executable.fromSubCommands(
      "tokenize" -> tokenize,
      "tree" -> tree,
      "time" -> time,
    )
  }

}
