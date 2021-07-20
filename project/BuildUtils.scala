import sbt._
import sbt.Keys._
import sbtassembly.AssemblyKeys._

object BuildUtils {}

/*
import klib.Implicits._
import klib.fp.types.{IO => KIO, _}
import klib.utils.{Logger => KLogger, _}
import klib.utils.Logger.{helpers => L}

object BuildUtils {

  private val latestReleaseFile = new File("build/latest-release.txt")

  private val kLogger = KLogger(KLogger.LogLevel.Info)

  private val buildTmplsImpl =
    Def.inputTask {
      def findFiles(
          file: File,
          rPkg: List[String],
          addSelfToPath: Boolean,
      ): KIO[List[(List[String], String)]] =
        // TODO (KR) : This needs to be wrapped in IO
        if (file.isDirectory)
          for {
            children <- file.listFiles.toList.pure[KIO]
            myRPkg = addSelfToPath ? (file.getName :: rPkg) | rPkg
            expanded <- children.map(findFiles(_, myRPkg, true)).traverse
          } yield expanded.flatten
        else if (file.isFile)
          for {
            contents <- KIO.readFile(file)
          } yield ((file.getName :: rPkg).reverse, contents) :: Nil
        else
          KIO.error(Message(s"Invalid file: $file"))

      {
        for {
          latestRelease <- KIO.readFile(latestReleaseFile)
          substitutions = List(
            // TODO (KR) : Switch to something similar for %version% (hook in with publish) (?)
            ("%version%", version.value),
            ("%latest-release%", latestRelease),
          )

          _ <- kLogger(
            L(
              L(substitutions.map(L.log.info(_))),
              L.break(),
            ),
          )

          tmplPairText <- KIO.readFile(new File("tmpl-map.txt"))
          tmplPairs <-
            tmplPairText
              .split("\n")
              .toList
              .zipWithIndex
              .map {
                case (str, line) =>
                  str.split(":").toList match {
                    case from :: to :: Nil => (new File(from), new File(to)).pure[?]
                    case _                 => ?.dead(Message(s"Invalid tmpl-map on line #${line + 1} [$str]"))
                  }
              }
              .traverse
              .toIO

          _ <- tmplPairs.map {
            case (from, to) =>
              for {
                _ <- kLogger(L.log.info(s"from: $from, to: $to"))
                filePairs <- findFiles(from, Nil, false)
                _ <- filePairs.map {
                  case (pkg, contents) =>
                    val fileStr = pkg.mkString("/")
                    val file = new File(to, fileStr).getAbsoluteFile

                    for {
                      _ <- kLogger(L.log.info(s"Generating: $fileStr -> $file"))
                      parentFile <- file.getParentFile.pure[KIO]
                      _ <- parentFile.mkdirs().pure[KIO]
                      _ <- KIO.writeFile(
                        file,
                        substitutions.foldLeft(contents) { case (current, (lit, rep)) => current.replaceAllLiterally(lit, rep) },
                      )
                    } yield ()
                }.traverse
                _ <- kLogger(L.break())
              } yield ()
          }.traverse
        } yield ()
      }.runSyncOrThrow(Some(kLogger))
    }

  val buildTmpls: Def.Setting[InputTask[Unit]] =
    InputKey[Unit]("buildTmpls") :=
      buildTmplsImpl.evaluated

  // TODO (KR) : Update this to use klib as well
  val buildJar: Def.Setting[InputTask[Unit]] =
    InputKey[Unit]("buildJar") :=
      Def.inputTaskDyn {
        Def.sequential(
          Def
            .inputTask {
              println("Running 'buildJar'...")
            }
            .toTask(""),
          assembly,
          Def
            .inputTask {
              IO.write(latestReleaseFile, version.value)
            }
            .toTask(""),
          buildTmplsImpl
            .toTask(""),
        )
      }.evaluated

}

 */
