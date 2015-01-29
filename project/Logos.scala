import sbt.Keys._
import sbt._
import scala.scalajs.sbtplugin.ScalaJSPlugin
import scala.scalajs.sbtplugin.ScalaJSCrossVersion
import scala.scalajs.sbtplugin.ScalaJSPlugin._


object Logos extends sbt.Build {

  lazy val scalaJsSettings = scalaJSBuildSettings ++ Seq(
    publishTo := (publishTo in ThisBuild).value
  )

  lazy val root = Project("logos", file(".")).dependsOn(
    collections, qlearning, pathing, utils
  ).aggregate(
    collections, qlearning, pathing, utils, rootJs
  )

  lazy val rootJs = project.in(file("js")).dependsOn(
    collectionsJs, qlearningJs, pathingJs, utilsJs
  ).aggregate(
    collectionsJs, qlearningJs, pathingJs, utilsJs
  ).settings (
    name := "logos",
    unmanagedSourceDirectories in Compile +=
        (baseDirectory in ThisBuild).value / "src" / "main" / "scala"
  ).settings(scalaJsSettings: _*)

  lazy val collections = project.in(file("collections")).settings(
    name := "logos-collections"
  )//.aggregate(collectionsJs)

  lazy val collectionsJs = project.in(file("collectionsJs")).settings (
    name := "logos-collections",
    unmanagedSourceDirectories in Compile +=
        (baseDirectory in collections).value / "src" / "main" / "scala"
  ).settings(scalaJsSettings: _*)

  lazy val qlearning = project.in(file("qlearning")).settings(
    name := "logos-qlearning"
  )//.aggregate(qlearningJs)

  lazy val qlearningJs = project.in(file("qlearningJs")).settings (
    name := "logos-qlearning",
    unmanagedSourceDirectories in Compile +=
        (baseDirectory in qlearning).value / "src" / "main" / "scala"
  ).settings(scalaJsSettings: _*)

  lazy val pathing = project.in(file("pathing")).settings(
    name := "logos-pathing"
  )//.aggregate(pathingJs)

  lazy val pathingJs = project.in(file("pathingJs")).settings (
    name := "logos-pathing",
    unmanagedSourceDirectories in Compile +=
        (baseDirectory in pathing).value / "src" / "main" / "scala"
  ).settings(scalaJsSettings: _*)

  lazy val utils = project.in(file("utils")).settings(
    name := "logos-utils"
  )//.aggregate(utilsJs)

  lazy val utilsJs = project.in(file("utilsJs")).settings (
    name := "logos-utils",
    unmanagedSourceDirectories in Compile +=
        (baseDirectory in utils).value / "src" / "main" / "scala"
  ).settings(scalaJsSettings: _*)

  override def rootProject = Some(root)

}