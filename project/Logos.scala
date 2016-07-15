import sbt.Keys._
import sbt._

import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin.autoImport._

import com.lihaoyi.workbench.Plugin._

object Logos {
  def userCredentials = (Path.userHome / ".ivy2" / "credentials" ** "*").filter(_.isFile).get.map(Credentials(_))

  val baseScalaVersion = "2.11.6"

  def buildSettings = Seq(
    name := "logos",
    scalaVersion := baseScalaVersion,
    organization := "com.colingodsey",

    publish := {},
    publishLocal := {},

    crossPaths in ThisBuild := true,
    crossScalaVersions in ThisBuild := Seq("2.12.0-M3", baseScalaVersion, "2.10.4"),

    scalacOptions in ThisBuild ++= Seq(scalaVersion.value match {
      case x if x.startsWith("2.12.") => "-target:jvm-1.8"
      case x => "-target:jvm-1.6"
    }),

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

  def commonSettings = Seq(
    //temp resolver for utest on 2.12.0-M3
    resolvers += "mvn repo" at "https://raw.githubusercontent.com/colinrgodsey/maven/master",
    resolvers += "mmreleases" at "https://artifactory.mediamath.com/artifactory/libs-release-global",
    libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1" % "test",
    testFrameworks += new TestFramework("utest.runner.Framework")
  ) ++ macroDeps

  def macroDeps = Seq(
    addCompilerPlugin("org.scalamacros" % s"paradise" % "2.1.0" cross CrossVersion.full),

    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <++= scalaVersion {
      case x if x.startsWith("2.10.") => Seq(
        "org.scalamacros" %% s"quasiquotes" % "2.0.0"
      )
      case x => Nil
    }
  )

  def macroSettings = Seq(name := "logos-macros") ++ macroDeps

  def akkaSettings = Seq(
    libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.15"
  )

  def claServerSettings = akkaSettings ++ Seq(
    resolvers += "Spray" at "http://repo.spray.io",
    libraryDependencies += "com.wandoulabs.akka" %% "spray-websocket" % "0.1.4"
  )

  def scalaJSONSettings = Seq(
    libraryDependencies += "com.mediamath" %%% "scala-json" % "1.0"
  )

  def domSettings = Seq(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0"
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