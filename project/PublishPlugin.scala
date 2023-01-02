import sbt.Keys._
import sbt._

/**
  * Automatically enriches projects with the following settings (despite the word "override").
  */
object PublishPlugin extends AutoPlugin {

  /**
    * Defines what members will be imported to the `build.sbt` scope.
    */
  val autoImport = ThingsToAutoImport

  /**
    * Thus plug-in will automatically be enabled; it has no requirements.
    */
  override def trigger: PluginTrigger = AllRequirements

  override val buildSettings: Seq[Setting[_]] = Seq(
    organization  := "com.htmlism",
    versionScheme := Some("strict"),
    publishTo     := Some("GitHub" at "https://maven.pkg.github.com/mcanlas/temporal-diagrams/"),
    credentials += Credentials(
      "GitHub Package Registry",
      "maven.pkg.github.com",
      "mcanlas",
      System.getenv("GITHUB_TOKEN")
    )
  )

  object ThingsToAutoImport {

    implicit class PublishOps(p: Project) {
      def disablePublshing: Project =
        p
          .settings(publish / skip := true)
    }
  }
}
