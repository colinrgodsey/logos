import sbt.Keys._

val mavenPath = file("./mvn")

val commonSettings = Seq(
  scalaVersion := "2.11.4",
  version := "0.2",
  organization := "com.colingodsey",
  crossPaths := true,
  publishTo := Some(Resolver.file("file", mavenPath))
)



//resolvers += Resolver.file("local mvn", mavenPath)

lazy val collections = Project("logos-collections", file("collections")).settings(commonSettings: _*)

lazy val qlearning = Project("logos-qlearning", file("qlearning")).settings(commonSettings: _*)

lazy val pathing = Project("logos-pathing", file("pathing")).settings(commonSettings: _*)

lazy val utils = Project("logos-utils", file("utils")).settings(commonSettings: _*)

lazy val root = Project("logos", file(".")).dependsOn(
  collections, qlearning, pathing, utils).settings(commonSettings: _*)
