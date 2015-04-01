import sbt.Keys._
import sbt._

import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object Logos {
  def buildSettings = Seq(
    name := "logos",

    publish := {},
    publishLocal := {},

    scalaVersion in ThisBuild := "2.11.4",
    organization in ThisBuild := "com.colingodsey",
    crossPaths in ThisBuild := true,
    crossScalaVersions in ThisBuild := Seq("2.11.5", "2.10.4"),

    publishTo in ThisBuild := Some(Resolver.file("file", file("../maven"))),

    version in ThisBuild <<= version in LocalRootProject,

    libraryDependencies in ThisBuild += "com.lihaoyi" %%% "utest" % "0.3.0" % "test",
    testFrameworks in ThisBuild += new TestFramework("utest.runner.Framework"),

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

}