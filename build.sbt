val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "composable-aggregations",
    organization := "dev.jp",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.12" % Test
    )
  )
