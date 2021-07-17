//

// =====|  |=====

val MyOrg = "io.github.kalin-rudnicki"
val githubUsername = "Kalin-Rudnicki"
val githubProject = "slyce-fp"

ThisBuild / organization := MyOrg
ThisBuild / organizationName := "kalin-rudnicki"
ThisBuild / organizationHomepage := Some(url(s"https://github.com/$githubUsername/$githubProject"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url(s"https://github.com/$githubUsername/$githubProject"),
    s"scm:git@github.com:$githubUsername/$githubProject.git",
  ),
)
ThisBuild / developers := List(
  Developer(
    id = "Kalin-Rudnicki",
    name = "Kalin Rudnicki",
    email = "kalin.rudnicki@gmail.com",
    url = url(s"https://github.com/$githubUsername"),
  ),
)

ThisBuild / description := "A (flex/bison)-esque parser generator for scala."
ThisBuild / licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT"))
ThisBuild / homepage := Some(url(s"https://github.com/$githubUsername/$githubProject"))

ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true

// =====|  |=====

val SharedSettings =
  Seq(
    organization := MyOrg,
    version := "1.1.1",
    scalaVersion := "2.13.4",
    resolvers += Resolver.mavenLocal,
  )

lazy val `slyce-core` =
  project
    .in(file("slyce-core"))
    .enablePlugins(BuildInfoPlugin)
    .settings(
      name := "slyce-core",
      SharedSettings,
      buildInfoKeys := Seq[BuildInfoKey](version),
      buildInfoPackage := "slyce",
      libraryDependencies += "kalin-rudnicki" %% "klib-core" % "0.5.11", // TODO (KR) :
      BuildUtils.buildReadme,
    )

lazy val `slyce-generate-parsers` =
  project
    .in(file("slyce-generate-parsers"))
    .settings(
      name := "slyce-generate-parsers",
      SharedSettings,
    )
    .dependsOn(
      `slyce-parse`,
    )

lazy val `slyce-generate` =
  project
    .in(file("slyce-generate"))
    .settings(
      name := "slyce-generate",
      SharedSettings,
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
      SharedSettings,
    )
    .dependsOn(
      `slyce-core`,
    )

lazy val `slyce-examples` =
  project
    .in(file("slyce-examples"))
    .settings(
      name := "slyce-examples",
      SharedSettings,
    )
    .dependsOn(
      `slyce-parse`,
    )
