import sbt.Keys.*
import sbt.*

object DependenciesPlugin extends AutoPlugin {
  override def trigger = allRequirements

  object autoImport {
    implicit class DependencyOps(p: Project) {
      def withCats: Project =
        p
          .settings(libraryDependencies += "org.typelevel" %% "cats-core" % Versions.catsCore)

      def withEffectMonad: Project =
        p
          .settings(libraryDependencies += "org.typelevel" %% "cats-effect" % Versions.catsEffect)

      def withHttpServer: Project =
        p
          .settings(
            libraryDependencies ++= Seq(
              "org.http4s" %% "http4s-dsl"          % Versions.http4s,
              "org.http4s" %% "http4s-ember-server" % Versions.http4s
            )
          )

      def withTesting: Project =
        p.settings(
          libraryDependencies ++= Seq(
            "org.typelevel" %% "weaver-cats"       % Versions.weaver % Test,
            "org.typelevel" %% "weaver-scalacheck" % Versions.weaver % Test
          )
        )
    }
  }
}
