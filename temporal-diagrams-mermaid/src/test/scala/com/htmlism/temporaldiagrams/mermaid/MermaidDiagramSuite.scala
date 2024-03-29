package com.htmlism.temporaldiagrams.mermaid

import cats.data.Chain
import cats.syntax.all.*
import weaver.FunSuite

import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl

object MermaidDiagramSuite extends FunSuite:
  test("Frontmatter is monoidal"):
    val foo =
      MermaidDiagram(
        Chain.one(FrontMatterPair.StringPair("foo", "123")),
        Flowchart.empty
      )

    val bar =
      MermaidDiagram(
        Chain.one(FrontMatterPair.StringPair("bar", "456")),
        Flowchart.empty
      )

    expect.same(
      MermaidDiagram(
        Chain(
          FrontMatterPair.StringPair("foo", "123"),
          FrontMatterPair.StringPair("bar", "456")
        ),
        Flowchart.empty
      ),
      foo |+| bar
    )

  test("Declarations are monoidal"):
    val foo =
      MermaidDiagram.of:
        Flowchart:
          FlowchartDsl.Node.Simple("foo")

    val bar =
      MermaidDiagram.of:
        Flowchart:
          FlowchartDsl.Node.Simple("bar")

    expect.same(
      MermaidDiagram.of:
        Flowchart(
          FlowchartDsl.Node.Simple("foo"),
          FlowchartDsl.Node.Simple("bar")
        )
      ,
      foo |+| bar
    )
