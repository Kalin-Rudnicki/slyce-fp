//

val SlyceVersion = "0.1.0"
val MyScalaVersion = "2.13.4"
val MyOrg = "kalin-rudnicki"

lazy val `slyce-core` =
  project
    .in(file("slyce-core"))
    .settings(
      organization := MyOrg,
      name := "slyce-core",
      version := SlyceVersion,
      scalaVersion := MyScalaVersion,
      resolvers += Resolver.mavenLocal,
      libraryDependencies += MyOrg %% "klib-core" % "0.1.8",
    )

lazy val `slyce-generate` =
  project
    .in(file("slyce-generate"))
    .settings(
      organization := MyOrg,
      name := "slyce-generate",
      version := SlyceVersion,
      scalaVersion := MyScalaVersion,
      resolvers += Resolver.mavenLocal,
      libraryDependencies ++= Seq(
        "com.lihaoyi" %% "scalatags" % "0.9.2",
      ),
    )
    .dependsOn(
      `slyce-core` % "compile->compile;test->test",
    )

lazy val `slyce-parse` =
  project
    .in(file("slyce-parse"))
    .settings(
      organization := MyOrg,
      name := "slyce-parse",
      version := SlyceVersion,
      scalaVersion := MyScalaVersion,
      resolvers += Resolver.mavenLocal,
    )
    .dependsOn(
      `slyce-core` % "compile->compile;test->test",
    )

lazy val `slyce-test` =
  project
    .in(file("slyce-test"))
    .settings(
      organization := MyOrg,
      name := "slyce-test",
      version := SlyceVersion,
      scalaVersion := MyScalaVersion,
      resolvers += Resolver.mavenLocal,
    )
    .dependsOn(
      `slyce-generate` % "compile->compile;test->test",
      `slyce-parse` % "compile->compile;test->test",
    )
