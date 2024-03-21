package com.htmlism.temporaldiagrams.mermaid

import com.htmlism.temporaldiagrams.mermaid.flowchart.*

object PrintCurveStyles extends App:
  val styles = List(
    CurveStyle.Basis,
    CurveStyle.BumpX,
    CurveStyle.BumpY,
    CurveStyle.Cardinal,
    CurveStyle.CatmullRom,
    CurveStyle.Linear,
    CurveStyle.MonotoneX,
    CurveStyle.MonotoneY,
    CurveStyle.Natural,
    CurveStyle.Step,
    CurveStyle.StepAfter,
    CurveStyle.StepBefore
  )

  println("```mermaid")
  println("```")
