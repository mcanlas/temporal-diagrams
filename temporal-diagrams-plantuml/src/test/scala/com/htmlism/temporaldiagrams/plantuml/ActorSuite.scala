package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import cats.syntax.all.*
import weaver.*

import com.htmlism.temporaldiagrams.v2.DiagramEncoder

object ActorSuite extends FunSuite:
  test("An actor has an name"):
    expect.eql(
      Chain("actor asdf"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Actor("asdf", None, None)
      )
    )

  test("An actor name escapes dashes"):
    expect.eql(
      Chain("actor \"as-df\""),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Actor("as-df", None, None)
      )
    )

  test("An actor name escapes spaces"):
    expect.eql(
      Chain("actor \"as df\""),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Actor("as df", None, None)
      )
    )

  test("An actor name escapes newlines"):
    expect.eql(
      Chain("actor \"as\\ndf\""),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Actor("as\ndf", None, None)
      )
    )

  test("An actor has an optional override alias"):
    expect.eql(
      Chain("actor foo as bar"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Actor("foo", "bar".some, None)
      )
    )

  test("An actor has an optional stereotype"):
    expect.eql(
      Chain("actor foo << bar >>"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Actor("foo", None, "bar".some)
      )
    )
