import sbt._
import sbt.Keys._
import sbtghactions._
import sbtghactions.GenerativeKeys._

/**
  * Automatically enriches projects with the following settings (despite the word "override").
  */
object GitHubActionsSettings extends AutoPlugin {

  /**
    * Thus plug-in will automatically be enabled; it has no requirements.
    */
  override def trigger: PluginTrigger = AllRequirements

  override def requires: Plugins =
    GitHubActionsPlugin

  override val buildSettings: Seq[Setting[_]] = Seq(
    versionScheme := Some("strict"),
    publishTo     := Some("GitHub" at "https://maven.pkg.github.com/mcanlas/temporal-diagrams/"),
    credentials += Credentials(
      "GitHub Package Registry",
      "maven.pkg.github.com",
      "mcanlas",
      sys.env("GH_PACKAGES_TOKEN")
    ),
    githubWorkflowBuild        := Seq(WorkflowStep.Sbt(List("scalafixAll --check", "scalafmtCheck", "test"))),
    githubWorkflowEnv          := Map("GH_PACKAGES_TOKEN" -> "${{ secrets.GH_PACKAGES_TOKEN }}"),
    githubWorkflowIncludeClean := false
  )
}
