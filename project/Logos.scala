import sbt.Keys._
import sbt._

import org.scalajs.sbtplugin.ScalaJSPlugin
import ScalaJSPlugin.autoImport._

import com.lihaoyi.workbench.Plugin._

object Logos {
  def buildSettings = Seq(
    name := "logos",
    scalaVersion := "2.11.4",
    organization := "com.colingodsey",

    publish := {},
    publishLocal := {},

    crossPaths in ThisBuild := true,
    crossScalaVersions in ThisBuild := Seq("2.11.5", "2.10.4"),

    publishTo in ThisBuild := Some(Resolver.file("file", file("../maven"))),

    version in ThisBuild <<= version in LocalRootProject,
    organization in ThisBuild <<= organization in LocalRootProject,
    scalaVersion in ThisBuild <<= scalaVersion in LocalRootProject,

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

  def claUISettings = workbenchSettings ++ Seq(
    libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.8.0",
    libraryDependencies += "be.doeraene" %%% "scalajs-pickling-core" % "0.4.0",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.7.0",

    skip in packageJSDependencies := false,
    bootSnippet := "com.colingodsey.logos.cla.ui.UI().main();",

    refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile),
    //updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile),

    jsDependencies += "org.webjars" % "flot" % "0.8.3-1" / "jquery.flot.js",
    jsDependencies += "org.webjars" % "chartjs" % "1.0.2" / "Chart.min.js"
  )
}