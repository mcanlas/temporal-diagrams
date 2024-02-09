package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import cats.syntax.all.*
import weaver.*

import com.htmlism.temporaldiagrams.DiagramEncoder

object ComponentSuite extends FunSuite:
  test("A component has an name"):
    expect.eql(
      Chain("component asdf"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Component("asdf", None, None)
      )
    )

  test("A component name escapes dashes"):
    expect.eql(
      Chain("component \"as-df\""),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Component("as-df", None, None)
      )
    )

  test("A component name escapes spaces"):
    expect.eql(
      Chain("component \"as df\""),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Component("as df", None, None)
      )
    )

  test("A component name escapes newlines"):
    expect.eql(
      Chain("component \"as\\ndf\""),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Component("as\ndf", None, None)
      )
    )

  test("A component has an optional override alias"):
    expect.eql(
      Chain("component foo as bar"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Component("foo", "bar".some, None)
      )
    )

  test("A component has an optional stereotype"):
    expect.eql(
      Chain("component foo << bar >>"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Component("foo", None, "bar".some)
      )
    )
