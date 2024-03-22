package com.htmlism.temporaldiagrams.mermaid
package flowchart

import cats.data.*
import weaver.FunSuite

import com.htmlism.temporaldiagrams.mermaid.flowchart.Flowchart.*
import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl.*

object StyleSuite extends FunSuite:
  test("Can declare styles for nodes"):
    expect.eql(
      Chain(
        "flowchart LR",
        "  id1(Start)",
        "",
        "  id2(Stop)",
        "",
        "  id1 --> id2"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart(
            // TODO add styles
            Node.Round("id1", "Start"),
            Node.Round("id2", "Stop"),
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
    )
