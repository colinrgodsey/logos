lazy val collections = crossProject.in(file("collections"))
    .settings(name := "logos-collections")
    .settings(Logos.commonSettings: _*)
    .jsSettings(scalaJSStage in Global := FastOptStage)
    .jvmSettings(scalacOptions ++= Seq("-optimise"))
lazy val collectionsJVM = collections.jvm.dependsOn(macros)
lazy val collectionsJs = collections.js.dependsOn(macros)

lazy val qlearning = crossProject.in(file("qlearning"))
    .settings(name := "logos-qlearning")
    .settings(Logos.commonSettings: _*)
    .dependsOn(collections)
lazy val qlearningJVM = qlearning.jvm
lazy val qlearningJs = qlearning.js

lazy val pathing = crossProject.in(file("pathing"))
    .settings(name := "logos-pathing")
    .settings(Logos.commonSettings: _*)
lazy val pathingJVM = pathing.jvm
lazy val pathingJs = pathing.js

lazy val utils = crossProject.in(file("utils"))
    .settings(name := "logos-utils")
    .settings(Logos.commonSettings: _*)
lazy val utilsJVM = utils.jvm
lazy val utilsJs = utils.js

lazy val akka = project.in(file("akka-js"))
    .settings(name := "logos-akka-js")
    .settings(Logos.commonSettings: _*)
    .enablePlugins(ScalaJSPlugin)

lazy val cla = crossProject.in(file("cla"))
    .settings(name := "logos-cla")
    .settings(Logos.commonSettings: _*)
    .dependsOn(collections, qlearning)
lazy val claJVM = cla.jvm
lazy val claJs = cla.js

lazy val macros = project.in(file("macros"))
    .settings(Logos.commonSettings: _*)
    .settings(
      name := "logos-macros",
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
    )

lazy val claServerInterface = crossProject.in(file("cla/server-interface"))
    .settings(name := "logos-cla-server-interface")
    .settings(Logos.commonSettings: _*)
    .settings(Logos.scalaJSONSettings: _*)
    .jvmSettings(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
    )
    .jvmSettings(Logos.akkaSettings: _*)
    .jsSettings(Logos.domSettings: _*)
    .dependsOn(cla)
lazy val claServerInterfaceJVM = claServerInterface.jvm
lazy val claServerInterfaceJs = claServerInterface.js

lazy val claServer = project.in(file("cla/server"))
    .settings(name := "logos-cla-server")
    .settings(Logos.commonSettings: _*)
    .settings(Logos.claServerSettings: _*)
    .dependsOn(claJVM, claServerInterfaceJVM)

lazy val root = crossProject.in(file("."))
    .settings(name := "logos")
    .settings(Logos.commonSettings: _*)
    .dependsOn(collections, qlearning, pathing, utils, cla)
lazy val rootJVM = root.jvm
lazy val rootJs = root.js

lazy val claUI = project.in(file("cla-ui"))
    .settings(name := "logos-cla-ui")
    .enablePlugins(ScalaJSPlugin)
    .settings(Logos.commonSettings: _*)
    .dependsOn(claJs, claServerInterfaceJs, akka)
    .settings(Logos.claUISettings: _*)

Logos.buildSettings
