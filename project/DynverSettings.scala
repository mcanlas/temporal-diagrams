import sbt.*
import sbt.Keys.*
import sbtdynver.DynVerPlugin.autoImport.*

object DynverSettings extends AutoPlugin {
  override def trigger: PluginTrigger = AllRequirements

  override def buildSettings: Seq[Setting[?]] =
    List(
      dynver ~= (_.replaceAll("0.0.0\\+", "")),
      version ~= (_.replaceAll("0.0.0\\+", ""))
    )
}
