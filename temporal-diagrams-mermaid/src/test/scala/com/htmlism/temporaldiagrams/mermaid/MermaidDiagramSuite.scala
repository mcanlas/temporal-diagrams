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
    expect.eql(
      "TODO",
      "TODO"
    )
