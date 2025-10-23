val scala3Version = "3.7.3"

lazy val alef = project
  .in(file("alef"))
  .settings( name          := "beklash"
    , version              := "0.1.0"
    , scalaVersion         := scala3Version
    , libraryDependencies ++=
      Seq("org.scalacheck" %% "scalacheck" % "1.19.0" % "test")
  )

lazy val root = project
  .in(file("."))
  .aggregate(alef)
