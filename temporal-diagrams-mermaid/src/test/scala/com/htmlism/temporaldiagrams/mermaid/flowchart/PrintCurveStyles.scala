package com.htmlism.temporaldiagrams.mermaid.flowchart

import cats.data.Chain
import cats.data.NonEmptyList
import cats.effect.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.mermaid.*
import com.htmlism.temporaldiagrams.mermaid.flowchart.*
import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl.Link.Head
import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl.Link.Head.Arrow

object PrintCurveStyles extends IOApp.Simple:
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
        FrontMatterPair.MapPair("config")(
          FrontMatterPair.MapPair("flowchart")(
            FrontMatterPair.StringPair.from("curve", style)
          )
        )
      ,
      Flowchart(
        FlowchartDsl.Node.Square("source", text       = style.s.some),
        FlowchartDsl.Node.Square("destinationA", text = None),
        FlowchartDsl.Node.Square("destinationB", text = None),
        FlowchartDsl
          .Link
          .LinkChain(
            NonEmptyList.one("source"),
            NonEmptyList.of(
              FlowchartDsl
                .Link
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
          ),
        FlowchartDsl
          .Link
          .LinkChain(
            NonEmptyList.one("source"),
            NonEmptyList.of(
              FlowchartDsl
                .Link
                .Segment
                .Visible(
                  2,
                  FlowchartDsl.Link.Weight.Normal,
                  FlowchartDsl.Link.Direction.Single(Head.Arrow),
                  text = None,
                  NonEmptyList.one("destinationB"),
                  None
                )
            )
          )
      )
    )

  def printStyle(s: CurveStyle) =
    MermaidDiagram
      .render(diagram(s))
      .prepend("```mermaid")
      .append("```")
      .traverse(std.Console[IO].println)

  def run: IO[Unit] =
    styles
      .traverse_(printStyle)