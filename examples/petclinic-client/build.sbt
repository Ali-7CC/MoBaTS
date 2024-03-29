version := "1.0.0"
name := "openapi-client"
organization := "org.openapitools"

scalaVersion := "2.13.5"
crossScalaVersions := Seq(scalaVersion.value, "2.12.13")

libraryDependencies ++= Seq(
    "com.softwaremill.sttp.client3" %% "core" % "3.8.2",
    "com.softwaremill.sttp.client3" %% "json4s" % "3.8.2",
    "org.json4s" %% "json4s-jackson" % "4.0.6"
)

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature"
)
