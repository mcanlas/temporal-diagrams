lazy val root =
  Project("temporal-diagrams", file(".")).withCats.withTesting

lazy val demo =
  project
    .dependsOn(root)
    .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.14")
