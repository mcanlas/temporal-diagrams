package com.htmlism.temporaldiagrams.mermaid.flowchart

sealed case class CurveStyle(s: String)

object CurveStyle:
  object Basis      extends CurveStyle("basis")
  object BumpX      extends CurveStyle("bumpX")
  object BumpY      extends CurveStyle("bumpY")
  object Cardinal   extends CurveStyle("cardinal")
  object CatmullRom extends CurveStyle("catmullRom")
  object Linear     extends CurveStyle("linear")
  object MonotoneX  extends CurveStyle("monotoneX")
  object MonotoneY  extends CurveStyle("monotoneY")
  object Natural    extends CurveStyle("natural")
  object Step       extends CurveStyle("step")
  object StepAfter  extends CurveStyle("stepAfter")
  object StepBefore extends CurveStyle("stepBefore")
