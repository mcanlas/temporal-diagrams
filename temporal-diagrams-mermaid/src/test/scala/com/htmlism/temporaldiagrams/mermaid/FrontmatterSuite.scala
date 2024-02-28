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
        MermaidDiagram[Flowchart](
          Chain.one:
            FrontmatterPair.StringPair("title", "asdf")
          ,
          Chain.empty
        )
      )
    )
