package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import cats.syntax.all.*
import weaver.*

import com.htmlism.temporaldiagrams.v2.DiagramEncoder

object LinkSuite extends FunSuite:
  test("A link has a source"):
    expect.eql(
      Chain(
        "foo --> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link(
          "foo",
          "bar",
          2,
          PlantUml.Link.Direction.Forwards,
          PlantUml.Link.Weight.Solid,
          text  = None,
          color = None
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
          "bar",
          2,
          PlantUml.Link.Direction.Forwards,
          PlantUml.Link.Weight.Solid,
          text  = None,
          color = None
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
          "bar",
          2,
          PlantUml.Link.Direction.Forwards,
          PlantUml.Link.Weight.Solid,
          text  = None,
          color = None
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
          "bar",
          2,
          PlantUml.Link.Direction.Forwards,
          PlantUml.Link.Weight.Solid,
          text  = None,
          color = None
        )
      )
    )

  test("A link supports a direction, backwards"):
    expect.eql(
      Chain(
        "foo <-- bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link(
          "foo",
          "bar",
          2,
          PlantUml.Link.Direction.Backwards,
          PlantUml.Link.Weight.Solid,
          text  = None,
          color = None
        )
      )
    )

  test("A link supports a direction, bidirectional"):
    expect.eql(
      Chain(
        "foo <--> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link(
          "foo",
          "bar",
          2,
          PlantUml.Link.Direction.Bidirectional,
          PlantUml.Link.Weight.Solid,
          text  = None,
          color = None
        )
      )
    )

  test("A link supports a direction, empty"):
    expect.eql(
      Chain(
        "foo -- bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link(
          "foo",
          "bar",
          2,
          PlantUml.Link.Direction.Empty,
          PlantUml.Link.Weight.Solid,
          text  = None,
          color = None
        )
      )
    )

  test("A link supports text"):
    expect.eql(
      Chain(
        "foo --> bar : comment"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link(
          "foo",
          "bar",
          2,
          PlantUml.Link.Direction.Forwards,
          PlantUml.Link.Weight.Solid,
          text  = "comment".some,
          color = None
        )
      )
    )

  test("A link supports a weight, solid"):
    expect.eql(
      Chain(
        "foo --> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link(
          "foo",
          "bar",
          2,
          PlantUml.Link.Direction.Forwards,
          PlantUml.Link.Weight.Solid,
          text  = None,
          color = None
        )
      )
    )

  test("A link supports a weight, dotted"):
    expect.eql(
      Chain(
        "foo ..> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link(
          "foo",
          "bar",
          2,
          PlantUml.Link.Direction.Forwards,
          PlantUml.Link.Weight.Dotted,
          text  = None,
          color = None
        )
      )
    )

  test("A link supports a weight, bold"):
    expect.eql(
      Chain(
        "foo -[bold]-> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link(
          "foo",
          "bar",
          2,
          PlantUml.Link.Direction.Forwards,
          PlantUml.Link.Weight.Bold,
          text  = None,
          color = None
        )
      )
    )

  test("A link supports a color"):
    expect.eql(
      Chain(
        "foo -[#red]-> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link(
          "foo",
          "bar",
          2,
          PlantUml.Link.Direction.Forwards,
          PlantUml.Link.Weight.Solid,
          text = None,
          "red".some
        )
      )
    )
