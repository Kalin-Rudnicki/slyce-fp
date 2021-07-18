//

// =====|  |=====

val Scala_2_12 = "2.12.10"
val Scala_2_13 = "2.13.4"

val MyOrg = "io.github.kalin-rudnicki"
val githubUsername = "Kalin-Rudnicki"
val githubProject = "slyce-fp"

ThisBuild / dynverVTagPrefix := false
ThisBuild / dynverSonatypeSnapshots := true

ThisBuild / version ~= (_.replace('+', '-'))
ThisBuild / dynver ~= (_.replace('+', '-'))

// =====|  |=====

inThisBuild(
  Seq(
    organization := MyOrg,
    resolvers += Resolver.mavenLocal,
    //
    description := "A (flex/bison)-esque parser generator for scala.",
    licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT")),
    developers := List(
      Developer(
        id = "Kalin-Rudnicki",
        name = "Kalin Rudnicki",
        email = "kalin.rudnicki@gmail.com",
        url = url(s"https://github.com/$githubUsername"),
      ),
    ),
  ),
)

val ScalaVersionSettings =
  Seq(
    scalaVersion := Scala_2_13,
    crossScalaVersions := Seq(Scala_2_12, Scala_2_13),
  )

// =====|  |=====

lazy val `slyce-core` =
  project
    .in(file("slyce-core"))
    .enablePlugins(BuildInfoPlugin)
    .settings(
      name := "slyce-core",
      ScalaVersionSettings,
      buildInfoKeys := Seq[BuildInfoKey](version),
      buildInfoPackage := "slyce",
      libraryDependencies += MyOrg %% "klib-core" % "0.6.3",
      BuildUtils.buildTmpls,
    )

lazy val `slyce-generate-parsers` =
  project
    .in(file("slyce-generate-parsers"))
    .settings(
      name := "slyce-generate-parsers",
      ScalaVersionSettings,
      slycePairs += SlyceConfig(SlyceInput.SrcDir, SlyceOutput.SrcDir),
    )
    .dependsOn(
      `slyce-parse`,
    )

lazy val `slyce-generate` =
  project
    .in(file("slyce-generate"))
    .settings(
      name := "slyce-generate",
      ScalaVersionSettings,
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "scalatags" % "0.9.2",
        "com.github.japgolly.scalacss" %% "ext-scalatags" % "0.7.0",
      ),
      assembly / assemblyJarName := s"slyce-generate-${version.value}.jar",
      BuildUtils.buildJar,
    )
    .dependsOn(
      `slyce-generate-parsers`,
    )

lazy val `slyce-parse` =
  project
    .in(file("slyce-parse"))
    .settings(
      name := "slyce-parse",
      ScalaVersionSettings,
    )
    .dependsOn(
      `slyce-core`,
    )

lazy val `slyce-examples` =
  project
    .in(file("slyce-examples"))
    .settings(
      name := "slyce-examples",
      ScalaVersionSettings,
      slycePairs += SlyceConfig(SlyceInput.SrcDir, SlyceOutput.SrcDir),
    )
    .dependsOn(
      `slyce-parse`,
    )

// NOTE : In order to get IntelliJ to be able to import project:
//      : 1) Un-Comment [1] & Comment [2]
//      : 2) Ctrl + Shift + O (or click the red reload button in top right)
//      : 3) Comment [1]    & Un-Comment [2]
//      : 4) Close the notification to try and re-import settings
lazy val `slyce-plugin` =
  project
    .in(file("slyce-plugin"))
    .enablePlugins(SbtPlugin)
    .settings(
      name := "slyce-plugin",
      scalaVersion := Scala_2_12,
      // libraryDependencies += MyOrg %% "slyce-generate" % "2.0.0", // [1]
    )
    .dependsOn(
      `slyce-generate`, // [2]
    )
