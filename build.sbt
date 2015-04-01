lazy val collections = crossProject.in(file("collections"))
  .settings(name := "logos-collections")
lazy val collectionsJVM = collections.jvm
lazy val collectionsJs = collections.js

lazy val qlearning = crossProject.in(file("qlearning"))
  .settings(name := "logos-qlearning")
  .dependsOn(collections)

lazy val qlearningJVM = qlearning.jvm
lazy val qlearningJs = qlearning.js

lazy val pathing = crossProject.in(file("pathing"))
  .settings(name := "logos-pathing")
lazy val pathingJVM = pathing.jvm
lazy val pathingJs = pathing.js

lazy val utils = crossProject.in(file("utils"))
  .settings(name := "logos-utils")
lazy val utilsJVM = utils.jvm
lazy val utilsJs = utils.js

Logos.buildSettings