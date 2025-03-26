lazy val root =
  Project("temporal-diagrams", file("."))
    .aggregate(core, demo, server, plantUml, mermaid, generate, generatePlantUml)

lazy val core =
  module("core")
    .settings(description := "A Scala DSL for generating PlantUML diagrams")
    .withCats
    .withTesting
    .enablePublishing

lazy val server =
  module("server")
    .settings(description := "A web service for visualizing temporal diagrams")
    .withHttpServer
    .withTesting
    .enablePublishing

lazy val demo =
  module("demo")
    .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.6.0")
    .dependsOn(core, mermaid, plantUml, generate)

lazy val plantUml =
  module("plantuml")
    .settings(description := "Temporal diagram encoders for PlantUML diagrams")
    .dependsOn(core)
    .withTesting
    .enablePublishing

lazy val mermaid =
  module("mermaid")
    .settings(description := "Temporal diagram encoders for Mermaid diagrams")
    .dependsOn(core)
    .withTesting
    .enablePublishing

lazy val generate =
  module("generate")
    .settings(description := "Tools for writing diagram code to disk")
    .withEffectMonad
    .enablePublishing

lazy val generatePlantUml =
  module("generate-plantuml")
    .settings(description := "Tools for rendering PlantUML images to disk")
    .dependsOn(generate)
    .enablePublishing
