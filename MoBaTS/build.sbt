val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "MoBaTS",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += 
      "com.softwaremill.sttp.client3" %% "core" % "3.8.2"
  )

  