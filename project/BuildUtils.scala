import sbt._
import sbt.Keys._
import sbtassembly.AssemblyKeys._

object BuildUtils {

  private val latestReleaseFile = new File("build/latest-release.txt")

  private val buildReadmeImpl =
    Def.inputTask {
      println("Building README.md...")

      val readmeTmpl = IO.read(new File("README.tmpl.md"))
      val latestRelease = IO.read(latestReleaseFile)

      // TODO (KR) : Switch to something similar for %version% (hook in with publish)
      val substituted =
        List(
          ("%version%", version.value),
          ("%latest-release%", latestRelease),
        ).foldLeft(readmeTmpl) {
          case (prev, (reg, str)) =>
            prev.replaceAllLiterally(reg, str)
        }
      IO.write(new File("README.md"), substituted)
    }

  val buildReadme: Def.Setting[InputTask[Unit]] =
    InputKey[Unit]("buildReadme") :=
      buildReadmeImpl.evaluated

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
          buildReadmeImpl
            .toTask(""),
        )
      }.evaluated

}
