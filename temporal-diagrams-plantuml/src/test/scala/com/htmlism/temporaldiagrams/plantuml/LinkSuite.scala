package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import cats.syntax.all.*
import weaver.*

import com.htmlism.temporaldiagrams.DiagramEncoder

object LinkSuite extends FunSuite:
  test("A link has a source"):
    expect.eql(
      Chain(
        "foo --> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link(
          "foo",
          "bar"
        )
      )
    )

  test("A link has a destination"):
    expect.eql(
      Chain(
        "foo --> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link(
          "foo",
          "bar"
        )
      )
    )

  test("A link has a length"):
    expect.eql(
      Chain(
        "foo --> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link(
          "foo",
          "bar"
        )
      )
    )

  test("A link supports a direction, forwards"):
    expect.eql(
      Chain(
        "foo --> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link(
          "foo",
          "bar"
        )
      )
    )

  test("A link supports a direction, backwards"):
    expect.eql(
      Chain(
        "foo <-- bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml
          .Link(
            "foo",
            "bar"
          )
          .copy(direction = PlantUml.Link.Direction.Backwards)
      )
    )

  test("A link supports a direction, bidirectional"):
    expect.eql(
      Chain(
        "foo <--> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml
          .Link(
            "foo",
            "bar"
          )
          .copy(direction = PlantUml.Link.Direction.Bidirectional)
      )
    )

  test("A link supports a direction, empty"):
    expect.eql(
      Chain(
        "foo -- bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml
          .Link(
            "foo",
            "bar"
          )
          .copy(direction = PlantUml.Link.Direction.Empty)
      )
    )

  test("A link supports text"):
    expect.eql(
      Chain(
        "foo --> bar : comment"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml
          .Link(
            "foo",
            "bar"
          )
          .withText("comment")
      )
    )

  test("A link supports a weight, solid"):
    expect.eql(
      Chain(
        "foo --> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml
          .Link(
            "foo",
            "bar"
          )
          .copy(weight = PlantUml.Link.Weight.Solid)
      )
    )

  test("A link supports a weight, dotted"):
    expect.eql(
      Chain(
        "foo ..> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml
          .Link(
            "foo",
            "bar"
          )
          .copy(weight = PlantUml.Link.Weight.Dotted)
      )
    )

  test("A link supports a weight, bold"):
    expect.eql(
      Chain(
        "foo -[bold]-> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml
          .Link(
            "foo",
            "bar"
          )
          .copy(weight = PlantUml.Link.Weight.Bold)
      )
    )

  test("A link supports a color"):
    expect.eql(
      Chain(
        "foo -[#red]-> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml
          .Link(
            "foo",
            "bar"
          )
          .copy(color = "red".some)
      )
    )

  test("A link supports disabling rank"):
    expect.eql(
      Chain(
        "foo -[norank]-> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml
          .Link(
            "foo",
            "bar"
          )
          .copy(influencesRank = false)
      )
    )

  test("A link supports being hidden"):
    expect.eql(
      Chain(
        "foo -[hidden]-> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml
          .Link(
            "foo",
            "bar"
          )
          .copy(isVisible = false)
      )
    )
