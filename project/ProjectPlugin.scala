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

    implicit class ProjectOps(p: Project) {
      val http4sVersion =
        "0.23.16"

      def withCats: Project =
        p
          .settings(libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0")

      def withHttpServer: Project =
        p
          .settings(
            libraryDependencies ++= Seq(
              "org.http4s" %% "http4s-dsl"          % http4sVersion,
              "org.http4s" %% "http4s-blaze-server" % http4sVersion
            )
          )

      def withTesting: Project =
        p.settings(libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test")
    }
  }
}
