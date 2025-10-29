val scala3Version = "3.7.3"

val testDeps = Seq(
  "org.scalatest"  %% "scalatest"  % "3.2.19" % "test",
  "org.scalacheck" %% "scalacheck" % "1.19.0" % "test"
)

lazy val util = project
  .in(file("util"))
  .settings(
    name                 := "beklash-util",
    scalaVersion         := scala3Version,
    libraryDependencies ++= testDeps
  )

lazy val alef = project
  .in(file("alef"))
  .dependsOn(util)
  .settings(
    name                 := "beklash-alef",
    scalaVersion         := scala3Version,
    libraryDependencies ++= testDeps,
  )

lazy val root = project
  .in(file("."))
  .aggregate(alef, util)
  .settings(
    name         := "beklash",
    version      := "0.1.0",
    scalaVersion := scala3Version
  )

Global / onChangedBuildSource := ReloadOnSourceChanges
