package com.htmlism.temporaldiagrams.mermaid
package flowchart

import cats.data.*
import cats.syntax.all.*
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
            Node.WithShape("id1", Node.Shape.Round, "Start".some),
            Node.WithShape("id2", Node.Shape.Round, "Stop".some),
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
                    NonEmptyList.one("id2")
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

  test("Class attachments are encoded after nodes are defined"):
    val expected =
      Chain(
        "flowchart",
        "  duck",
        "",
        "  class duck foo"
      )

    val nodeIdAlphabeticallyGreaterThanClass =
      "duck"

    val mermaid =
      MermaidDiagram(
        Chain.empty,
        Flowchart(
          ClassAttachment(NonEmptyList.of("duck"), "foo"),
          Node.Simple(nodeIdAlphabeticallyGreaterThanClass)
        )
      )

    expect.eql(expected, MermaidDiagram.render(mermaid))
