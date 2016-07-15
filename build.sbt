lazy val collections = crossProject.in(file("collections"))
    .settings(name := "logos-collections")
    .settings(Logos.commonSettings: _*)
    .jsSettings(
      jsEnv := NodeJSEnv().value,

      scalaJSStage in Global := FastOptStage
    )
    .jvmSettings(fork in Test := true)
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

lazy val cla = crossProject.in(file("cla"))
    .settings(name := "logos-cla")
    .settings(Logos.commonSettings: _*)
    .dependsOn(collections, qlearning, akkajs)
lazy val claJVM = cla.jvm
lazy val claJs = cla.js

lazy val macros = project.in(file("macros"))
    .settings(name := "logos-macros")
    .settings(Logos.commonSettings: _*)
    .settings(Logos.macroSettings: _*)

lazy val akkajs = crossProject.in(file("akkajs"))
    .settings(name := "logos-akkajs")
    .settings(Logos.commonSettings: _*)
    .settings(Logos.scalaJSONSettings: _*)
    .jvmSettings(Logos.akkaSettings: _*)
    .jsSettings(Logos.domSettings: _*)
lazy val akkajsJVM = akkajs.jvm
lazy val akkajsJs = akkajs.js

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
    .dependsOn(claJs, claServerInterfaceJs, akkajsJs)
    .settings(Logos.claUISettings: _*)

Logos.buildSettings
