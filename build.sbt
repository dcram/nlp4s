import Dependencies._
lazy val scala213 = "2.13.8"
//lazy val scala212 = "2.12.16"
ThisBuild / scalaVersion     := scala213
ThisBuild / version          := "0.5.0"
ThisBuild / organization     := "fr.dcram"
ThisBuild / organizationName := "dcram"
lazy val supportedScalaVersions = List(scala213)
ThisBuild / crossScalaVersions := supportedScalaVersions

lazy val root = (project in file("."))
  .settings(
    name := "nlp4s",
    libraryDependencies += slf4j,
    libraryDependencies += collectionCompat,
      publishTo := {
      val nexus = "https://nexus.secure.sparklane/"
      if (isSnapshot.value)
          Some("snapshots" at nexus + "repository/snapshots/")
      else
          Some("releases"  at nexus + "repository/releases/")
    },
    credentials += Credentials(Path.userHome / ".sbt" / ".credentials"),

    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaLogging % Test,
    libraryDependencies += logback % Test,
//    libraryDependencies += "org.typelevel" %% "cats-core" % "2.1.1" % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
