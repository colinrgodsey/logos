scalaVersion := "2.11.4"

version := "0.1"

organization := "com.colingodsey"

crossPaths := true

publishTo := Some(Resolver.file("file", new File("./mvn")))

lazy val collections = Project("collections", file("collections"))

lazy val qlearning = Project("qlearning", file("qlearning"))

lazy val pathing = Project("pathing", file("pathing"))

lazy val utils = Project("utils", file("utils"))

lazy val root = Project("logos", file("."))
