val scala3Version = "3.1.2"
val fs2Version = "3.2.8"
val circeVersion = "0.14.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "composable-aggregations",
    organization := "dev.jp",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.8.0",
      "org.typelevel" %% "cats-effect" % "3.3.13",
      "co.fs2" %% "fs2-core" % fs2Version,
      "co.fs2" %% "fs2-io" % fs2Version,
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "org.scalatest" %% "scalatest" % "3.2.12" % Test,
      "org.scalatestplus" %% "scalacheck-1-16" % "3.2.12.0" % Test,
      "org.scalacheck" %% "scalacheck" % "1.16.0" % Test
    )
  )
