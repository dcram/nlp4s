import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.12"
  lazy val logback = "ch.qos.logback" % "logback-classic" % "1.2.11"
  lazy val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
  lazy val slf4j =  "org.slf4j" % "slf4j-api" % "1.7.36"
  lazy val collectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % "2.7.0"

}
