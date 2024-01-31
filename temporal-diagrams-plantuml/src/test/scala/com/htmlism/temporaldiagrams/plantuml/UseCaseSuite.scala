package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import cats.syntax.all.*
import weaver.*

import com.htmlism.temporaldiagrams.v2.DiagramEncoder

object UseCaseSuite extends FunSuite:
  test("A use case has an name"):
    expect.eql(
      Chain("usecase asdf"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.UseCase("asdf", None, None)
      )
    )

  test("A use case name escapes dashes"):
    expect.eql(
      Chain("usecase \"as-df\""),
      DiagramEncoder[PlantUml].encode(
        PlantUml.UseCase("as-df", None, None)
      )
    )

  test("A use case name escapes spaces"):
    expect.eql(
      Chain("usecase \"as df\""),
      DiagramEncoder[PlantUml].encode(
        PlantUml.UseCase("as df", None, None)
      )
    )

  test("A use case name escapes newlines"):
    expect.eql(
      Chain("usecase \"as\\ndf\""),
      DiagramEncoder[PlantUml].encode(
        PlantUml.UseCase("as\ndf", None, None)
      )
    )

  test("A use case has an optional override alias"):
    expect.eql(
      Chain("usecase foo as bar"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.UseCase("foo", "bar".some, None)
      )
    )

  test("A use case has an optional stereotype"):
    expect.eql(
      Chain("usecase foo << bar >>"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.UseCase("foo", None, "bar".some)
      )
    )
