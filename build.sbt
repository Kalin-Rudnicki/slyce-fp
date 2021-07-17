//

// =====|  |=====

val Scala_2_12 = "2.12.10"
val Scala_2_13 = "2.13.4"

val MyOrg = "io.github.kalin-rudnicki"
val githubUsername = "Kalin-Rudnicki"
val githubProject = "slyce-fp"

// =====|  |=====

val ScalaVersionSettings =
  Seq(
    scalaVersion := Scala_2_13,
    crossScalaVersions := Seq(Scala_2_12, Scala_2_13),
  )

val SharedSettings =
  Seq(
    organization := MyOrg,
    version := "1.1.3",
    resolvers += Resolver.mavenLocal,
  )

val PublishSettings =
  Seq(
    //
    description := "A (flex/bison)-esque parser generator for scala.",
    licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT")),
    homepage := Some(url(s"https://github.com/$githubUsername/$githubProject")),
    //
    organizationName := "kalin-rudnicki",
    organizationHomepage := Some(url(s"https://github.com/$githubUsername/$githubProject")),
    //
    scmInfo := Some(
      ScmInfo(
        url(s"https://github.com/$githubUsername/$githubProject"),
        s"scm:git@github.com:$githubUsername/$githubProject.git",
      ),
    ),
    developers := List(
      Developer(
        id = "Kalin-Rudnicki",
        name = "Kalin Rudnicki",
        email = "kalin.rudnicki@gmail.com",
        url = url(s"https://github.com/$githubUsername"),
      ),
    ),
    //
    pomIncludeRepository := { _ => false },
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    publishMavenStyle := true,
  )

lazy val `slyce-core` =
  project
    .in(file("slyce-core"))
    .enablePlugins(BuildInfoPlugin)
    .settings(
      name := "slyce-core",
      SharedSettings,
      ScalaVersionSettings,
      buildInfoKeys := Seq[BuildInfoKey](version),
      buildInfoPackage := "slyce",
      libraryDependencies += MyOrg %% "klib-core" % "0.6.3",
      BuildUtils.buildReadme,
    )

lazy val `slyce-generate-parsers` =
  project
    .in(file("slyce-generate-parsers"))
    .settings(
      name := "slyce-generate-parsers",
      SharedSettings,
      ScalaVersionSettings,
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
      SharedSettings,
      ScalaVersionSettings,
      PublishSettings,
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
      ScalaVersionSettings,
    )
    .dependsOn(
      `slyce-parse`,
    )

lazy val `slyce-plugin` =
  project
    .in(file("slyce-plugin"))
    .enablePlugins(SbtPlugin)
    .settings(
      name := "slyce-plugin",
      SharedSettings,
      scalaVersion := Scala_2_12,
    )
