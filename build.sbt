import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "fr.dcram"
ThisBuild / organizationName := "dcram"

lazy val root = (project in file("."))
  .settings(
    name := "nlp4s",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaLogging,
    libraryDependencies += logback % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
