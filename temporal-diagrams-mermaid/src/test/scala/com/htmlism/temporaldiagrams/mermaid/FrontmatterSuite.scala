package com.htmlism.temporaldiagrams.mermaid

import cats.data.Chain
import weaver.FunSuite

object FrontmatterSuite extends FunSuite:
  test("Empty Frontmatter renders empty"):
    expect.eql(
      Chain.one:
        "flowchart"
      ,
      MermaidDiagram.render(MermaidDiagram.empty[Flowchart])
    )

  test("Non-empty Frontmatter renders"):
    expect.eql(
      Chain(
        "---",
        "title: asdf",
        "---",
        "flowchart"
      ),
      MermaidDiagram.render(
        MermaidDiagram(
          Chain.one:
            FrontMatterPair.StringPair("title", "asdf")
          ,
          Flowchart.empty
        )
      )
    )

  test("A string pair renders"):
    expect.eql(
      Chain(
        "---",
        "title: asdf",
        "---",
        "flowchart"
      ),
      MermaidDiagram.render(
        MermaidDiagram(
          Chain.one:
            FrontMatterPair.StringPair("title", "asdf")
          ,
          Flowchart.empty
        )
      )
    )

  test("A map renders"):
    expect.eql(
      Chain(
        "---",
        "asdf:",
        "  foo: 123",
        "  bar: 456",
        "---",
        "flowchart"
      ),
      MermaidDiagram.render(
        MermaidDiagram(
          Chain.one:
            FrontMatterPair.MapPair(
              "asdf",
              Chain(
                FrontMatterPair.StringPair("foo", "123"),
                FrontMatterPair.StringPair("bar", "456")
              )
            )
          ,
          Flowchart.empty
        )
      )
    )
