import sbt.Keys.*
import sbt.*

/**
  * Automatically enriches projects with the following settings (despite the word "override").
  */
object Scala2Plugin extends AutoPlugin {

  /**
    * Thus plug-in will automatically be enabled; it has no requirements.
    */
  override def trigger: PluginTrigger = AllRequirements

  override val buildSettings: Seq[Setting[?]] = Seq(
    scalaVersion := "2.13.12"
  )

  override val projectSettings: Seq[Setting[?]] = Seq(
    scalacOptions ++= Seq("-Xsource:3")
  )
}
