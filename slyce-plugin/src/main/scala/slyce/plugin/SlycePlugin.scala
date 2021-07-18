package slyce.plugin

import sbt._
import sbt.Keys._

import klib.Implicits._
import klib.fp.types._
import klib.utils.{Logger => KLogger, _}
import klib.utils.Logger.{helpers => L}

import slyce.generate.main._

object SlycePlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {

    val slyce: TaskKey[Unit] = taskKey("slyce")
    val slycePairs: SettingKey[Seq[SlyceConfig]] = settingKey("slycePairs")

    sealed trait SlyceInput
    object SlyceInput {
      case object SrcDir extends SlyceInput
      final case class Dir(file: File) extends SlyceInput
    }

    sealed trait SlyceOutput
    object SlyceOutput {
      case object SrcDir extends SlyceOutput
      case object SourcesManaged extends SlyceOutput
      final case class Dir(file: File) extends SlyceOutput
    }

    sealed trait SlyceDebugOutput
    object SlyceDebugOutput {
      case object Target extends SlyceDebugOutput
      final case class Dir(file: File) extends SlyceDebugOutput
    }

    final case class SlyceConfig(
        input: SlyceInput,
        output: SlyceOutput,
        nameMap: Option[String] = scala.None,
        tokenize: Boolean = false,
        slyceDebugOutput: Option[SlyceDebugOutput] = scala.Some(SlyceDebugOutput.Target),
    )

  }

  import autoImport._

  override def globalSettings: Seq[Def.Setting[_]] =
    Seq(
      slycePairs := Seq(),
    )

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      slyce := {
        val slycePairValues = slycePairs.value

        // TODO (KR) : Get some other way... (?)
        val kLogger = KLogger(KLogger.LogLevel.Info)

        {
          for {
            _ <- slycePairValues.nonEmpty.maybe(kLogger(L.log.info(s"Running slyce in ${name.value}..."))).traverse

            _ <- slycePairValues.toList.map {
              slyceConfig =>
                Main
                  .findPairsAndGenerate(
                    logger = kLogger,
                    inputDir = slyceConfig.input match {
                      case SlyceInput.SrcDir    => sourceDirectory.value / "main" / "slyce"
                      case SlyceInput.Dir(file) => file
                    },
                    outputDir = slyceConfig.output match {
                      case SlyceOutput.SrcDir         => sourceDirectory.value / "main" / "scala"
                      case SlyceOutput.SourcesManaged => sourceManaged.value
                      case SlyceOutput.Dir(file)      => file
                    },
                    nameMap = slyceConfig.nameMap match {
                      case scala.Some(value) => Some(value)
                      case scala.None        => None
                    },
                    tokenize = slyceConfig.tokenize,
                    debugOutputDir = slyceConfig.slyceDebugOutput.map {
                      case SlyceDebugOutput.Target    => target.value / "slyce-debug"
                      case SlyceDebugOutput.Dir(file) => file
                    } match {
                      case scala.Some(value) => Some(value)
                      case scala.None        => None
                    },
                  )
            }.traverse
          } yield ()
        }.runSyncOrThrow(Some(kLogger))
      },
    )

}
