lazy val root =
  Project("temporal-diagrams", file("."))
    .withCats
    .withTesting
    .aggregate(core, demo, server)
    .disablePublshing

lazy val core =
  module("core")
    .withCats
    .withTesting

lazy val server =
  module("server")
    .withHttpServer
    .withTesting

lazy val demo =
  module("demo")
    .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.1")
    .dependsOn(core)
    .disablePublshing
