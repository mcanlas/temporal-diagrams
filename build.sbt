lazy val root =
  Project("temporal-diagrams", file("."))
    .withCats
    .withTesting
    .aggregate(core, demo, server, plantUml, mermaid)
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
    .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.1")
    .dependsOn(core)
    .disablePublshing

lazy val plantUml =
  module("plantuml")
    .settings(description := "Temporal diagram encoders for PlantUML diagrams")
    .dependsOn(core)
    .withTesting

lazy val mermaid =
  module("mermaid")
    .settings(description := "Temporal diagram encoders for Mermaid diagrams")
    .dependsOn(core)
    .withTesting
