package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import cats.syntax.all.*
import weaver.*

import com.htmlism.temporaldiagrams.v2.DiagramEncoder

object ComponentSuite extends FunSuite:
  test("A component has an name"):
    expect.eql(
      Chain("component asdf"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Component("asdf", None, None)
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
