//

val MyOrg = "kalin-rudnicki"
val SharedSettings =
  Seq(
    organization := MyOrg,
    version := "0.1.0",
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
      libraryDependencies += MyOrg %% "klib-core" % "0.5.11",
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

lazy val `slyce-test` =
  project
    .in(file("slyce-test"))
    .settings(
      name := "slyce-test",
      SharedSettings,
    )
    .dependsOn(
      `slyce-generate`,
      `slyce-parse`,
    )
