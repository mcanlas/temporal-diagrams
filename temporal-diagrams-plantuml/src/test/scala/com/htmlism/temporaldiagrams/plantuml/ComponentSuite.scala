package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import cats.syntax.all.*
import weaver.*

import com.htmlism.temporaldiagrams.v2.DiagramEncoder

object ComponentSuite extends FunSuite {
  test("A component has an name") {
    expect.eql(
      NonEmptyChain.one("component asdf"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Component("asdf", None, None)
      )
    )
  }

  test("A component has an optional override alias") {
    expect.eql(
      NonEmptyChain.one("component foo as bar"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Component("foo", "bar".some, None)
      )
    )
  }

  test("A component has an optional stereotype") {
    expect.eql(
      NonEmptyChain.one("component foo << bar >>"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Component("foo", None, "bar".some)
      )
    )
  }
}
