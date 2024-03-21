package com.htmlism.temporaldiagrams.mermaid

import cats.data.Chain
import cats.data.NonEmptyList

import com.htmlism.temporaldiagrams.mermaid.*
import com.htmlism.temporaldiagrams.mermaid.flowchart.*
import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl.Link.Head
import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl.Link.Head.Arrow

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

  def diagram(style: CurveStyle): MermaidDiagram[Flowchart] =
    MermaidDiagram(
      Chain.one:
        FrontMatterPair.MapPair(
          "config",
          Chain.one:
            FrontMatterPair.MapPair(
              "frontmatter",
              Chain.one:
                FrontMatterPair.StringPair.from("curve", style)
            )
        )
      ,
      Flowchart(
        FlowchartDsl.Node.Square("source", text       = None),
        FlowchartDsl.Node.Square("destinationA", text = None),
        FlowchartDsl.Node.Square("destinationB", text = None),
        FlowchartDsl
          .Link
          .LinkChain(
            NonEmptyList.one("source"),
            NonEmptyList.one:
              FlowchartDsl
                .Link
                .LinkChain
                .Segment
                .Visible(
                  2,
                  FlowchartDsl.Link.Weight.Normal,
                  FlowchartDsl.Link.Direction.Single(Head.Arrow),
                  text = None,
                  NonEmptyList.one("destinationA"),
                  None
                )
          )
      )
    )

  println("```mermaid")
  println("```")
