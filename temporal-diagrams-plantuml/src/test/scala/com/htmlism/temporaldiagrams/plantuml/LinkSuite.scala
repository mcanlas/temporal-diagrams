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
        PlantUml.Link("foo", "bar", 2, PlantUml.Link.Direction.Forwards, text = None)
      )
    )

  test("A link has a destination"):
    expect.eql(
      Chain(
        "foo --> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link("foo", "bar", 2, PlantUml.Link.Direction.Forwards, text = None)
      )
    )

  test("A link has a length"):
    expect.eql(
      Chain(
        "foo --> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link("foo", "bar", 2, PlantUml.Link.Direction.Forwards, text = None)
      )
    )

  test("A link supports a direction, forwards"):
    expect.eql(
      Chain(
        "foo --> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link("foo", "bar", 2, PlantUml.Link.Direction.Forwards, text = None)
      )
    )

  test("A link supports a direction, backwards"):
    expect.eql(
      Chain(
        "foo <-- bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link("foo", "bar", 2, PlantUml.Link.Direction.Backwards, text = None)
      )
    )

  test("A link supports a direction, bidirectional"):
    expect.eql(
      Chain(
        "foo <--> bar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link("foo", "bar", 2, PlantUml.Link.Direction.Bidirectional, text = None)
      )
    )

  test("A link supports text"):
    expect.eql(
      Chain(
        "foo --> bar : comment"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Link("foo", "bar", 2, PlantUml.Link.Direction.Forwards, text = "comment".some)
      )
    )
