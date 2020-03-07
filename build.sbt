import Dependencies._
lazy val scala212 = "2.12.8"
lazy val scala211 = "2.11.12"
ThisBuild / scalaVersion     := scala212
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "fr.dcram"
ThisBuild / organizationName := "dcram"
lazy val supportedScalaVersions = List(scala212, scala211)
ThisBuild / crossScalaVersions := supportedScalaVersions

lazy val root = (project in file("."))
  .settings(
    name := "nlp4s",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaLogging % Test,
    libraryDependencies += logback % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
