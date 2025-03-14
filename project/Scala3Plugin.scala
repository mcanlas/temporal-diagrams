import sbt.Keys.*
import sbt.*

/**
  * Automatically enriches projects with the following settings (despite the word "override").
  */
object Scala3Plugin extends AutoPlugin {

  /**
    * Thus plug-in will automatically be enabled; it has no requirements.
    */
  override def trigger: PluginTrigger = AllRequirements

  override val buildSettings: Seq[Setting[?]] = Seq(
    scalaVersion := "3.6.4"
  )

  override val projectSettings: Seq[Setting[?]] = Seq(
    scalacOptions ++= Seq("-indent", "-rewrite", "-source:3.7")
  )
}
