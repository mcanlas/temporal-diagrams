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
      MermaidDiagram(
        Chain.empty,
        Flowchart:
          FlowchartDsl.Node.Square("foo")
      )

    val bar =
      MermaidDiagram(
        Chain.empty,
        Flowchart:
          FlowchartDsl.Node.Square("bar")
      )

    expect.same(
      MermaidDiagram(
        Chain.empty,
        Flowchart(
          FlowchartDsl.Node.Square("foo"),
          FlowchartDsl.Node.Square("bar")
        )
      ),
      foo |+| bar
    )
