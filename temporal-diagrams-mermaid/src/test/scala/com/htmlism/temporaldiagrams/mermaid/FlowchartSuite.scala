package com.htmlism.temporaldiagrams.mermaid

import cats.data.Chain
import weaver.FunSuite

object FlowchartSuite extends FunSuite:
  test("Can render an empty flowchart"):
    expect.eql(
      Chain.one:
        "flowchart"
      ,
      MermaidDiagram.render(MermaidDiagram.empty[Flowchart])
    )

  test("Can render an empty flowchart, left to right"):
    expect.eql(
      Chain.one:
        "flowchart LR"
      ,
      MermaidDiagram.render(MermaidDiagram.empty[Flowchart.LR])
    )

  test("Can render an empty flowchart, right to left"):
    expect.eql(
      Chain.one:
        "flowchart RL"
      ,
      MermaidDiagram.render(MermaidDiagram.empty[Flowchart.RL])
    )

  test("Can render an empty flowchart, top down"):
    expect.eql(
      Chain.one:
        "flowchart TD"
      ,
      MermaidDiagram.render(MermaidDiagram.empty[Flowchart.TD])
    )

  test("Can render an empty flowchart, top to bottom"):
    expect.eql(
      Chain.one:
        "flowchart TB"
      ,
      MermaidDiagram.render(MermaidDiagram.empty[Flowchart.TB])
    )

  test("Can render an empty flowchart, bottom to top"):
    expect.eql(
      Chain.one:
        "flowchart BT"
      ,
      MermaidDiagram.render(MermaidDiagram.empty[Flowchart.BT])
    )
