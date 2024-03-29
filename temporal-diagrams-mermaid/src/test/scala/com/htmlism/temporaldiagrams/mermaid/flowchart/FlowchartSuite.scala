package com.htmlism.temporaldiagrams.mermaid
package flowchart

import cats.data.Chain
import cats.data.NonEmptyList
import cats.syntax.all.*
import weaver.FunSuite

import com.htmlism.temporaldiagrams.mermaid.flowchart.Flowchart.*
import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl.*

object FlowchartSuite extends FunSuite:
  test("Can render an empty flowchart"):
    expect.eql(
      Chain.one:
        "flowchart"
      ,
      MermaidDiagram.render:
        MermaidDiagram.empty[Flowchart]
    )

  test("Can render an empty flowchart, left to right"):
    expect.eql(
      Chain.one:
        "flowchart LR"
      ,
      MermaidDiagram.render:
        MermaidDiagram.of(Flowchart.empty.withDirection(Direction.LR))
    )

  test("Can render an empty flowchart, right to left"):
    expect.eql(
      Chain.one:
        "flowchart RL"
      ,
      MermaidDiagram.render:
        MermaidDiagram.of(Flowchart.empty.withDirection(Direction.RL))
    )

  test("Can render an empty flowchart, top down"):
    expect.eql(
      Chain.one:
        "flowchart TD"
      ,
      MermaidDiagram.render:
        MermaidDiagram.of(Flowchart.empty.withDirection(Direction.TD))
    )

  test("Can render an empty flowchart, top to bottom"):
    expect.eql(
      Chain.one:
        "flowchart TB"
      ,
      MermaidDiagram.render:
        MermaidDiagram.of(Flowchart.empty.withDirection(Direction.TB))
    )

  test("Can render an empty flowchart, bottom to top"):
    expect.eql(
      Chain.one:
        "flowchart BT"
      ,
      MermaidDiagram.render:
        MermaidDiagram.of(Flowchart.empty.withDirection(Direction.BT))
    )

  test("A component diagram can be built with varargs style or a foldable collection"):
    val xs =
      List(
        Node.Simple("foo", None, None),
        Node.Simple("bar", None, None)
      )

    val byVarargs =
      Flowchart(xs*)

    val byFoldable =
      Flowchart(xs)

    expect.same(byVarargs, byFoldable)

  test("Supports nodes, without text"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.Simple("foo")
    )

  test("Supports nodes, with text"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[bar]"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.Simple("foo", text = Some("bar"))
    )

  test("Can render node shape: square"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[bar]"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.WithShape(id = "foo", Node.Shape.Square, text = "bar".some)
    )

  test("Can render node shape: round"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo(bar)"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.WithShape(id = "foo", Node.Shape.Round, text = "bar".some)
    )

  test("Can render node shape: stadium"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo([bar])"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.WithShape(id = "foo", Node.Shape.Stadium, text = "bar".some)
    )

  test("Can render node shape: subroutine"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[[bar]]"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.WithShape(id = "foo", Node.Shape.Subroutine, text = "bar".some)
    )

  test("Can render node shape: cylinder"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[(bar)]"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.WithShape(id = "foo", Node.Shape.Cylinder, text = "bar".some)
    )

  test("Can render node shape: circle"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo((bar))"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.WithShape(id = "foo", Node.Shape.Circle, text = "bar".some)
    )

  test("Can render node shape: asymmetric"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo>bar]"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.WithShape(id = "foo", Node.Shape.Asymmetric, text = "bar".some)
    )

  test("Can render node shape: rhombus"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo{bar}"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.WithShape(id = "foo", Node.Shape.Rhombus, text = "bar".some)
    )

  test("Can render node shape: hexagon"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo{{bar}}"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.WithShape(id = "foo", Node.Shape.Hexagon, text = "bar".some)
    )

  test("Can render node shape: parallelogram"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[/bar/]"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.WithShape(id = "foo", Node.Shape.Parallelogram, text = "bar".some)
    )

  test("Can render node shape: parallelogram alt"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[\\bar\\]"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.WithShape(id = "foo", Node.Shape.ParallelogramAlt, text = "bar".some)
    )

  test("Can render node shape: trapezoid"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[/bar\\]"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.WithShape(id = "foo", Node.Shape.Trapezoid, text = "bar".some)
    )

  test("Can render node shape: trapezoid alt"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo[\\bar/]"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.WithShape(id = "foo", Node.Shape.TrapezoidAlt, text = "bar".some)
    )

  test("Can render node shape: double circle"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo(((bar)))"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Node.WithShape(id = "foo", Node.Shape.DoubleCircle, text = "bar".some)
    )

  test("Can render a link: open with lengths and text"):
    expect.eql(
      Chain(
        "flowchart",
        "  alpha --- beta ---- gamma"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Link.LinkChain(
              NonEmptyList.one("alpha"),
              NonEmptyList.of(
                Link
                  .Segment
                  .Visible(
                    1,
                    Link.Weight.Normal,
                    Link.Direction.Open,
                    NonEmptyList.one("beta")
                  ),
                Link
                  .Segment
                  .Visible(
                    2,
                    Link.Weight.Normal,
                    Link.Direction.Open,
                    NonEmptyList.one("gamma")
                  )
              )
            )
    )

  test("Can render a link: arrow"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo --> bar"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Link.LinkChain(
              NonEmptyList.one("foo"),
              NonEmptyList.one(
                Link
                  .Segment
                  .Visible(
                    1,
                    Link.Weight.Normal,
                    Link.Direction.Single(Link.Head.Arrow),
                    NonEmptyList.one("bar")
                  )
              )
            )
    )

  test("Can render a link: multi direction"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo <--> bar"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Link.LinkChain(
              NonEmptyList.one("foo"),
              NonEmptyList.one(
                Link
                  .Segment
                  .Visible(
                    1,
                    Link.Weight.Normal,
                    Link.Direction.Multi(Link.Head.Arrow),
                    NonEmptyList.one("bar")
                  )
              )
            )
    )

  test("Can render a link: with text"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo -- hello --- bar"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Link.LinkChain(
              NonEmptyList.one("foo"),
              NonEmptyList.one(
                Link
                  .Segment
                  .Visible(
                    1,
                    Link.Weight.Normal,
                    Link.Direction.Open,
                    NonEmptyList.one("bar"),
                    text = Some("hello")
                  )
              )
            )
    )

  test("Can render a link: invisible segment, with lengths"):
    expect.eql(
      Chain(
        "flowchart",
        "  foo ~~~ bar ~~~~ baz"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Link.LinkChain(
              NonEmptyList.one("foo"),
              NonEmptyList.of(
                Link
                  .Segment
                  .Invisible(
                    1,
                    NonEmptyList.one("bar")
                  ),
                Link
                  .Segment
                  .Invisible(
                    2,
                    NonEmptyList.one("baz")
                  )
              )
            )
    )

  test("Can render a link: dotted weight, no text"):
    expect.eql(
      Chain(
        "flowchart",
        "  one -.- two -..- three"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Link.LinkChain(
              NonEmptyList.one("one"),
              NonEmptyList.of(
                Link
                  .Segment
                  .Visible(
                    1,
                    Link.Weight.Dotted,
                    Link.Direction.Open,
                    NonEmptyList.one("two")
                  ),
                Link
                  .Segment
                  .Visible(
                    2,
                    Link.Weight.Dotted,
                    Link.Direction.Open,
                    NonEmptyList.one("three")
                  )
              )
            )
    )

  test("Can render a link: dotted weight, with text"):
    expect.eql(
      Chain(
        "flowchart",
        "  one -. foo .- two -. bar ..- three"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Link.LinkChain(
              NonEmptyList.one("one"),
              NonEmptyList.of(
                Link
                  .Segment
                  .Visible(
                    1,
                    Link.Weight.Dotted,
                    Link.Direction.Open,
                    NonEmptyList.one("two"),
                    text = Some("foo")
                  ),
                Link
                  .Segment
                  .Visible(
                    2,
                    Link.Weight.Dotted,
                    Link.Direction.Open,
                    NonEmptyList.one("three"),
                    text = Some("bar")
                  )
              )
            )
    )

  test("Can render a link: with styles, simple example"):
    expect.eql(
      Chain(
        "flowchart",
        "  one -- this has style --> two",
        "  linkStyle 0 stroke:#ff3, stroke-width:4px"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Link.LinkChain(
              NonEmptyList.one("one"),
              NonEmptyList.of(
                Link
                  .Segment
                  .Visible(
                    1,
                    Link.Weight.Normal,
                    Link.Direction.Single(Link.Head.Arrow),
                    NonEmptyList.one("two"),
                    text = Some("this has style"),
                    style = NonEmptyList
                      .of(
                        StyleDeclaration("stroke", "#ff3"),
                        StyleDeclaration(
                          "stroke-width",
                          "4px"
                        )
                      )
                      .some
                  )
              )
            )
    )

  test("Can render a link: with style; with multiple segments, skips empty"):
    expect.eql(
      Chain(
        "flowchart",
        "  one -- foo --> two -- bar ---> three",
        "  linkStyle 1 stroke:#3ff, stroke-width:2px"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart:
            Link.LinkChain(
              NonEmptyList.one("one"),
              NonEmptyList.of(
                Link
                  .Segment
                  .Visible(
                    1,
                    Link.Weight.Normal,
                    Link.Direction.Single(Link.Head.Arrow),
                    NonEmptyList.one("two"),
                    text = Some("foo")
                  ),
                Link
                  .Segment
                  .Visible(
                    2,
                    Link.Weight.Normal,
                    Link.Direction.Single(Link.Head.Arrow),
                    NonEmptyList.one("three"),
                    text = Some("bar"),
                    style = NonEmptyList
                      .of(
                        StyleDeclaration("stroke", "#3ff"),
                        StyleDeclaration(
                          "stroke-width",
                          "2px"
                        )
                      )
                      .some
                  )
              )
            )
    )

  test("Can render a link: with style; with multiple link groups, skips empty"):
    expect.eql(
      Chain(
        "flowchart",
        "  one -- foo --> two -- bar ---> three",
        "",
        "  styledStart --> styledEnd",
        "  linkStyle 2 stroke:#3ff, stroke-width:2px"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart(
            Link.LinkChain(
              NonEmptyList.one("one"),
              NonEmptyList.of(
                Link
                  .Segment
                  .Visible(
                    1,
                    Link.Weight.Normal,
                    Link.Direction.Single(Link.Head.Arrow),
                    NonEmptyList.one("two"),
                    text = Some("foo")
                  ),
                Link
                  .Segment
                  .Visible(
                    2,
                    Link.Weight.Normal,
                    Link.Direction.Single(Link.Head.Arrow),
                    NonEmptyList.one("three"),
                    text = Some("bar")
                  )
              )
            ),
            Link.LinkChain(
              NonEmptyList.one("styledStart"),
              NonEmptyList.one(
                Link
                  .Segment
                  .Visible(
                    1,
                    Link.Weight.Normal,
                    Link.Direction.Single(Link.Head.Arrow),
                    NonEmptyList.one("styledEnd"),
                    text = None,
                    style = NonEmptyList
                      .of(
                        StyleDeclaration("stroke", "#3ff"),
                        StyleDeclaration(
                          "stroke-width",
                          "2px"
                        )
                      )
                      .some
                  )
              )
            )
          )
    )

  test("Can render a subgraph"):
    expect.eql(
      Chain(
        "flowchart",
        "  subgraph bar [barsub]",
        "  end",
        "",
        "  subgraph foo",
        "  end"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart(
            Subgraph("foo", None, None, Set.empty, Set.empty),
            Subgraph("bar", Some("barsub"), None, Set.empty, Set.empty)
          )
    )

  test("Can render a subgraph, with direction"):
    expect.eql(
      Chain(
        "flowchart",
        "  subgraph foo",
        "    direction RL",
        "  end"
      ),
      MermaidDiagram.render:
        MermaidDiagram.of:
          Flowchart(
            Subgraph("foo", None, Some(Subgraph.Direction.RL), Set.empty, Set.empty)
          )
    )
