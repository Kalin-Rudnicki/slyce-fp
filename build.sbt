//

val SlyceVersion = "0.1.0"
val MyScalaVersion = "2.12.10"

lazy val `slyce-core` =
  project
    .in(file("slyce-core"))
    .settings(
      name := "slyce-core",
      version := SlyceVersion,
      scalaVersion := MyScalaVersion,
    )

lazy val `slyce-generate` =
  project
    .in(file("slyce-generate"))
    .settings(
      name := "slyce-generate",
      version := SlyceVersion,
      scalaVersion := MyScalaVersion,
    )
    .dependsOn(`slyce-core`)

lazy val `slyce-parse` =
  project
    .in(file("slyce-parse"))
    .settings(
      name := "slyce-parse",
      version := SlyceVersion,
      scalaVersion := MyScalaVersion,
    )
    .dependsOn(`slyce-core`)

lazy val `slyce-test` =
  project
    .in(file("slyce-test"))
    .settings(
      name := "slyce-test",
      version := SlyceVersion,
      scalaVersion := MyScalaVersion,
    )
    .dependsOn(`slyce-generate`, `slyce-parse`)
