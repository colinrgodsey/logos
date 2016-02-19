import sbt.Keys._
import sbt._

import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin.autoImport._

import com.lihaoyi.workbench.Plugin._

object Logos {
  def userCredentials = (Path.userHome / ".ivy2" / "credentials" ** "*").filter(_.isFile).get.map(Credentials(_))

  def buildSettings = Seq(
    name := "logos",
    scalaVersion := "2.11.6",
    organization := "com.colingodsey",

    publish := {},
    publishLocal := {},

    crossPaths in ThisBuild := true,
    crossScalaVersions in ThisBuild := Seq("2.11.6", "2.10.4"),

    publishTo in ThisBuild := Some(Resolver.file("file", file("../maven"))),

    version in ThisBuild <<= version in LocalRootProject,
    organization in ThisBuild <<= organization in LocalRootProject,
    scalaVersion in ThisBuild <<= scalaVersion in LocalRootProject,

    scalacOptions in ThisBuild ++= Seq("-unchecked", "-feature"),

    localUrl := ("localhost", 12345),

    credentials ++= userCredentials,

    pomExtra in ThisBuild :=
      <url>https://github.com/colinrgodsey/logos</url>
        <licenses>
          <license>
            <name></name>
            <url></url>
          </license>
        </licenses>
        <scm>
          <url>git://github.com/colinrgodsey/logos.git</url>
          <connection>scm:git://github.com/colinrgodsey/logos.git</connection>
        </scm>
        <developers>
          <developer>
            <id>colinrgodsey</id>
            <name>Colin Godsey</name>
            <url>https://github.com/colinrgodsey/</url>
          </developer>
        </developers>
  )

  //todo: ditch?
  def commonSettings = Seq(
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.0" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework")
  )

  def akkaSettings = Seq(
    libraryDependencies +=
        "com.typesafe.akka" %% "akka-actor" % "2.3.9"
  )

  def claServerSettings = akkaSettings ++ Seq(
    resolvers += "Spray" at "http://repo.spray.io",
    libraryDependencies += "com.wandoulabs.akka" %% "spray-websocket" % "0.1.4"
  )

  def scalaJSONSettings = Seq(
    libraryDependencies += "com.mediamath" %%% "scala-json" % "0.2-SNAPSHOT"
  )

  def domSettings = Seq(
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.8.0"
    )
  )

  def claUISettings = workbenchSettings ++ scalaJSONSettings ++ domSettings ++ Seq(
    resolvers += "mmreleases" at
      "https://artifactory.mediamath.com/artifactory/libs-release",

    jsDependencies ++= Seq(
      "org.webjars" % "jquery" % "2.1.3" / "2.1.3/jquery.js",
      "org.webjars" % "flot" % "0.8.3-1" / "jquery.flot.js",
      "org.webjars" % "chartjs" % "1.0.2" / "Chart.min.js"
    ),

    localUrl := ("localhost", 12345),

    skip in packageJSDependencies := false,
    bootSnippet := "__createGame();",

    //refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile)
    updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile)

  )
}