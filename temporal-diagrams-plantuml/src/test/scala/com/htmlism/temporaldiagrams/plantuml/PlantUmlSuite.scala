package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import com.htmlism.temporaldiagrams.v2.DiagramEncoder
import weaver.*

object PlantUmlSuite extends FunSuite {
  test("Can render one component") {
    expect.eql(
      NonEmptyChain.of("@startuml", "", "component asdf", "", "@enduml"),
      PlantUml.render(
        NonEmptyChain.one(
          PlantUml.Component("asdf", None, None)
        )
      )
    )
  }

  test("Can render many components, AND lexicographically sorts them") {
    val xs =
      NonEmptyChain
        .of[PlantUml](
          PlantUml.Component("foo", None, None),
          PlantUml.Component("bar", None, None)
        )

    expect.eql(
      NonEmptyChain.of("@startuml", "", "component bar", "", "component foo", "", "@enduml"),
      PlantUml.render(xs)
    )
  }

  test("Can render the left to right directive") {
    expect.eql(
      NonEmptyChain.one("left to right direction"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.LeftToRightDirection
      )
    )
  }
}
