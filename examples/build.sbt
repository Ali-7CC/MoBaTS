val scala3Version = "3.2.1"

lazy val petClinicClient = Project(id="petclinic-client", base = file("petclinic-client"))

lazy val root = project
  .in(file("."))
  .dependsOn(petClinicClient)
  .aggregate(petClinicClient)
  .settings(
    name := "examples",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++=  Seq(
      ("mobats" %% "mobats" % "0.1.0-SNAPSHOT").exclude("com.softwaremill.sttp.client3", "core_3"),
        "org.scalatest" %% "scalatest-funsuite" % "3.2.14"
      )
    )
