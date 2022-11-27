import sbt._
import sbt.Keys._
import sbtdynver.DynVerPlugin.autoImport._

object DynverSettings extends AutoPlugin {
  override def trigger: PluginTrigger = AllRequirements

  override def buildSettings: Seq[Setting[_]] =
    List(
      dynver ~= (_.replaceAll("0.0.0\\+", "")),
      version ~= (_.replaceAll("0.0.0\\+", ""))
    )
}
