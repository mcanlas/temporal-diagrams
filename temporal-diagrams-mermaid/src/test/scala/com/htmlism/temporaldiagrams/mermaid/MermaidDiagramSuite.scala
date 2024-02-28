package com.htmlism.temporaldiagrams.mermaid

import cats.data.Chain
import cats.syntax.all.*
import weaver.FunSuite

import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDeclaration

object MermaidDiagramSuite extends FunSuite:
  test("Frontmatter is monoidal"):
    val foo =
      MermaidDiagram(
        Chain.one(FrontmatterPair.StringPair("foo", "123")),
        Flowchart.empty
      )

    val bar =
      MermaidDiagram(
        Chain.one(FrontmatterPair.StringPair("bar", "456")),
        Flowchart.empty
      )

    expect.same(
      MermaidDiagram(
        Chain(
          FrontmatterPair.StringPair("foo", "123"),
          FrontmatterPair.StringPair("bar", "456")
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
          Flowchart.Node.Square("foo", text = None)
      )

    val bar =
      MermaidDiagram(
        Chain.empty,
        Flowchart:
          Flowchart.Node.Square("bar", text = None)
      )

    expect.same(
      MermaidDiagram(
        Chain.empty,
        Flowchart(
          Flowchart.Node.Square("foo", text = None),
          Flowchart.Node.Square("bar", text = None)
        )
      ),
      foo |+| bar
    )
