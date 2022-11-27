lazy val root =
  Project("temporal-diagrams", file("."))
    .withCats
    .withTesting
    .aggregate(core, demo, server)
    .disablePublshing

lazy val core =
  project
    .settings(name := "temporal-diagrams-core")
    .withCats
    .withTesting

lazy val server =
  project
    .settings(name := "temporal-diagrams-server")
    .withHttpServer

lazy val demo =
  project
    .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.1")
    .dependsOn(core)
