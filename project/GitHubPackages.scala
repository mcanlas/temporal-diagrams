import sbt.*
import sbt.Keys.*

/**
  * Automatically enriches projects with the following settings (despite the word "override").
  */
object GitHubPackages extends AutoPlugin {

  /**
    * Thus plug-in will automatically be enabled; it has no requirements.
    */
  override def trigger: PluginTrigger = AllRequirements

  val autoImport = ThingsToAutoImport

  object ThingsToAutoImport {
    implicit class PackagesOps(p: Project) {
      def withGitHubPackagesCredentials: Project =
        p
          .settings(
            credentials += Credentials(
              "GitHub Package Registry",
              "maven.pkg.github.com",
              "mcanlas",
              sys.env("GH_PACKAGES_TOKEN")
            )
          )

      def withResolver(project: String): Project =
        p
          .settings(
            resolvers += s"mcanlas/$project" at s"https://maven.pkg.github.com/mcanlas/$project/"
          )
    }
  }
}
