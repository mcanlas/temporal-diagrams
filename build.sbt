lazy val root =
  Project("temporal-diagrams", file("."))
    .withCats
    .withTesting
    .aggregate(core, demo)

lazy val core =
  project
    .settings(name := "temporal-diagrams-core")
    .withCats
    .withTesting

lazy val interactive =
  project
    .settings(name := "temporal-diagrams-interactive")
    .withHttpServer

lazy val demo =
  project
    .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.0")
    .dependsOn(core)
