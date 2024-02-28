package com.htmlism.temporaldiagrams.mermaid

import cats.data.Chain
import cats.syntax.all.*
import weaver.FunSuite

object MermaidDiagramSuite extends FunSuite:
  test("Frontmatter is monoidal"):
    val foo =
      MermaidDiagram[Flowchart](
        Chain.one(FrontmatterPair.StringPair("foo", "123")),
        Chain.empty
      )

    val bar =
      MermaidDiagram[Flowchart](
        Chain.one(FrontmatterPair.StringPair("bar", "456")),
        Chain.empty
      )

    expect.same(
      MermaidDiagram[Flowchart](
        Chain(
          FrontmatterPair.StringPair("foo", "123"),
          FrontmatterPair.StringPair("bar", "456")
        ),
        Chain.empty
      ),
      foo |+| bar
    )

  test("Declarations are monoidal"):
    val foo =
      MermaidDiagram[Flowchart](
        Chain.empty,
        Chain.one:
          Flowchart.Node("foo", text = None)
      )

    val bar =
      MermaidDiagram[Flowchart](
        Chain.empty,
        Chain.one:
          Flowchart.Node("bar", text = None)
      )

    expect.same(
      MermaidDiagram[Flowchart](
        Chain.empty,
        Chain(
          Flowchart.Node("foo", text = None),
          Flowchart.Node("bar", text = None)
        )
      ),
      foo |+| bar
    )
