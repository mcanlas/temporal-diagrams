import sbt.Keys._
import sbt._

/**
  * Automatically enriches projects with the following settings (despite the word "override").
  */
object ProjectPlugin extends AutoPlugin {

  /**
    * Defines what members will be imported to the `build.sbt` scope.
    */
  val autoImport = ThingsToAutoImport

  /**
    * Thus plug-in will automatically be enabled; it has no requirements.
    */
  override def trigger: PluginTrigger = AllRequirements

  object ThingsToAutoImport {
    private def jarName(s: String) =
      "temporal-diagrams-" + s

    def module(s: String): Project =
      Project(s, file(jarName(s)))
        .settings(name := jarName(s))
        .settings(addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full))

    implicit class ProjectOps(p: Project) {
      val http4sVersion =
        "0.23.18"

      def withCats: Project =
        p
          .settings(libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0")

      def withHttpServer: Project =
        p
          .settings(
            libraryDependencies ++= Seq(
              "org.http4s" %% "http4s-dsl"          % http4sVersion,
              "org.http4s" %% "http4s-blaze-server" % "0.23.13"
            )
          )

      def withTesting: Project = {
        val weaverVersion =
          "0.8.3"

        p.settings(
          testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
          libraryDependencies ++= Seq(
            "com.disneystreaming" %% "weaver-cats"       % weaverVersion % Test,
            "com.disneystreaming" %% "weaver-scalacheck" % weaverVersion % Test
          )
        )
      }
    }
  }
}
