package com.htmlism.temporaldiagrams.mermaid
package flowchart

import cats.data.*
import weaver.FunSuite

import com.htmlism.temporaldiagrams.mermaid.flowchart.Flowchart.*
import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl.*

object StyleSuite extends FunSuite:
  test("Can declare styles for nodes"):
    val mermaid =
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart(
            Node.Round("id1", "Start"),
            Node.Round("id2", "Stop"),
            Style("id1", "fill" -> "#f9f", "stroke" -> "#333", "stroke-width" -> "4px"),
            Style(
              "id2",
              "fill"             -> "#bbf",
              "stroke"           -> "#f66",
              "stroke-width"     -> "2px",
              "color"            -> "#fff",
              "stroke-dasharray" -> "5 5"
            ),
            Link.LinkChain(
              NonEmptyList.one("id1"),
              NonEmptyList.of(
                Link
                  .Segment
                  .Visible(
                    1,
                    Link.Weight.Normal,
                    Link.Direction.Single(Link.Head.Arrow),
                    text = None,
                    NonEmptyList.one("id2"),
                    style = None
                  )
              )
            )
          )
            .withDirection(Flowchart.Direction.LR)
        )

    expect.eql(
      Chain(
        "flowchart LR",
        "  id1(Start)",
        "",
        "  id2(Stop)",
        "",
        "  style id1 fill:#f9f,stroke:#333,stroke-width:4px",
        "",
        "  style id2 fill:#bbf,stroke:#f66,stroke-width:2px,color:#fff,stroke-dasharray:5 5",
        "",
        "  id1 --> id2"
      ),
      mermaid
    )

  test("Can define classes"):
    val expected =
      Chain(
        "flowchart",
        "  classDef alpha,beta a:b,c:d"
      )

    val mermaid =
      MermaidDiagram(
        Chain.empty,
        Flowchart(
          ClassDef(NonEmptyList.of("alpha", "beta"), NonEmptyList.of("a" -> "b", "c" -> "d"))
        )
      )

    expect.eql(expected, MermaidDiagram.render(mermaid))

  test("Can attach classes to nodes with declarations"):
    val expected =
      Chain(
        "flowchart",
        "  class alpha,beta foo"
      )

    val mermaid =
      MermaidDiagram(
        Chain.empty,
        Flowchart(
          ClassAttachment(NonEmptyList.of("alpha", "beta"), "foo")
        )
      )

    expect.eql(expected, MermaidDiagram.render(mermaid))
