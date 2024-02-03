import sbt.Keys.*
import sbt.*

object DependenciesPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    implicit class DependencyOps(p: Project) {
      val http4sVersion =
        "0.23.25"

      def withCats: Project =
        p
          .settings(libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0")

      def withEffectMonad: Project =
        p
          .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.3")

      def withHttpServer: Project =
        p
          .settings(
            libraryDependencies ++= Seq(
              "org.http4s" %% "http4s-dsl"          % http4sVersion,
              "org.http4s" %% "http4s-blaze-server" % "0.23.16"
            )
          )

      def withTesting: Project = {
        val weaverVersion =
          "0.8.4"

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
