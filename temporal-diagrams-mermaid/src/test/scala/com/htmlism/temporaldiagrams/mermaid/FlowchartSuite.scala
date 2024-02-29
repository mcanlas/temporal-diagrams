package com.htmlism.temporaldiagrams.mermaid

import cats.data.Chain
import cats.data.NonEmptyList
import weaver.FunSuite

import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl
import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl.*

object FlowchartSuite extends FunSuite:
  test("Can render an empty flowchart"):
    expect.eql(
      Chain.one:
        "flowchart"
      ,
      MermaidDiagram.render(MermaidDiagram.empty[Flowchart])
    )

  test("Can render an empty flowchart, left to right"):
    expect.eql(
      Chain.one:
        "flowchart LR"
      ,
      MermaidDiagram.render(MermaidDiagram.empty[Flowchart.LR])
    )

  test("Can render an empty flowchart, right to left"):
    expect.eql(
      Chain.one:
        "flowchart RL"
      ,
      MermaidDiagram.render(MermaidDiagram.empty[Flowchart.RL])
    )

  test("Can render an empty flowchart, top down"):
    expect.eql(
      Chain.one:
        "flowchart TD"
      ,
      MermaidDiagram.render(MermaidDiagram.empty[Flowchart.TD])
    )

  test("Can render an empty flowchart, top to bottom"):
    expect.eql(
      Chain.one:
        "flowchart TB"
      ,
      MermaidDiagram.render(MermaidDiagram.empty[Flowchart.TB])
    )

  test("Can render an empty flowchart, bottom to top"):
    expect.eql(
      Chain.one:
        "flowchart BT"
      ,
      MermaidDiagram.render(MermaidDiagram.empty[Flowchart.BT])
    )

  test("Supports nodes, without text"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.Square("foo", text = None)
        )
    )

  test("Supports nodes, with text"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[bar]"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.Square("foo", text = Some("bar"))
        )
    )

  test("Can render node shape: round"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo(bar)"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.Round(id = "foo", text = "bar")
        )
    )

  test("Can render node shape: stadium"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo([bar])"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.Stadium(id = "foo", text = "bar")
        )
    )

  test("Can render node shape: subroutine"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[[bar]]"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.Subroutine(id = "foo", text = "bar")
        )
    )

  test("Can render node shape: cylinder"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[(bar)]"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.Cylinder(id = "foo", text = "bar")
        )
    )

  test("Can render node shape: circle"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo((bar))"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.Circle(id = "foo", text = "bar")
        )
    )

  test("Can render node shape: asymmetric"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo>bar]"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.Asymmetric(id = "foo", text = "bar")
        )
    )

  test("Can render node shape: rhombus"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo{bar}"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.Rhombus(id = "foo", text = "bar")
        )
    )

  test("Can render node shape: hexagon"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo{{bar}}"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.Hexagon(id = "foo", text = "bar")
        )
    )

  test("Can render node shape: parallelogram"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[/bar/]"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.Parallelogram(id = "foo", text = "bar")
        )
    )

  test("Can render node shape: parallelogram alt"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[\\bar\\]"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.ParallelogramAlt(id = "foo", text = "bar")
        )
    )

  test("Can render node shape: trapezoid"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[/bar\\]"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.Trapezoid(id = "foo", text = "bar")
        )
    )

  test("Can render node shape: trapezoid alt"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[\\bar/]"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.TrapezoidAlt(id = "foo", text = "bar")
        )
    )

  test("Can render node shape: double circle"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo(((bar)))"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Node.DoubleCircle(id = "foo", text = "bar")
        )
    )

  test("Can render a link: open"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo --- bar"
      ),
      MermaidDiagram.render:
        MermaidDiagram(
          Chain.empty,
          Flowchart:
            Link.LinkChain(
              NonEmptyList.one("foo"),
              NonEmptyList.one(
                Link
                  .LinkChain
                  .Segment(
                    Link.Weight.Normal,
                    Link.Direction.Open,
                    text = None,
                    NonEmptyList.one("bar")
                  )
              )
            )
        )
    )
