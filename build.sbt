import sbt.Keys._

scalaVersion in ThisBuild := "2.11.4"

version in ThisBuild := "0.4"

organization in ThisBuild := "com.colingodsey"

crossPaths in ThisBuild := true

crossScalaVersions in ThisBuild := Seq("2.11.4", "2.10.4")

publish in ThisBuild := ()

publishTo in ThisBuild := Some(Resolver.file("file", file("../maven")))

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
