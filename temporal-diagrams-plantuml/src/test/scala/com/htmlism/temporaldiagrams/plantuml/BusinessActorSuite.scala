package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import cats.syntax.all.*
import weaver.*

import com.htmlism.temporaldiagrams.DiagramEncoder

object BusinessActorSuite extends FunSuite:
  test("A business actor has an name"):
    expect.eql(
      Chain("actor/ asdf"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.BusinessActor("asdf", None, None)
      )
    )

  test("A business actor name escapes dashes"):
    expect.eql(
      Chain("actor/ \"as-df\""),
      DiagramEncoder[PlantUml].encode(
        PlantUml.BusinessActor("as-df", None, None)
      )
    )

  test("A business actor name escapes spaces"):
    expect.eql(
      Chain("actor/ \"as df\""),
      DiagramEncoder[PlantUml].encode(
        PlantUml.BusinessActor("as df", None, None)
      )
    )

  test("A business actor name escapes newlines"):
    expect.eql(
      Chain("actor/ \"as\\ndf\""),
      DiagramEncoder[PlantUml].encode(
        PlantUml.BusinessActor("as\ndf", None, None)
      )
    )

  test("A business actor has an optional override alias"):
    expect.eql(
      Chain("actor/ foo as bar"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.BusinessActor("foo", "bar".some, None)
      )
    )

  test("A business actor has an optional stereotype"):
    expect.eql(
      Chain("actor/ foo << bar >>"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.BusinessActor("foo", None, "bar".some)
      )
    )
