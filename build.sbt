lazy val root =
  Project("temporal-diagrams", file("."))
    .withCats
    .withTesting
    .aggregate(core, demo, server)
    .disablePublshing

lazy val core =
  module("core")
    .settings(description := "A Scala DSL for generating PlantUML diagrams")
    .withCats
    .withTesting

lazy val server =
  module("server")
    .settings(description := "A web service for visualizing temporal diagrams")
    .withHttpServer
    .withTesting

lazy val demo =
  module("demo")
    .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.9")
    .dependsOn(core)
    .disablePublshing
