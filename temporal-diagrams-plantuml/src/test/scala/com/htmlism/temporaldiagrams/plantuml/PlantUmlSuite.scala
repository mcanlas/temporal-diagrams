package com.htmlism.temporaldiagrams.plantuml

import cats.data._
import weaver._

object PlantUmlSuite extends FunSuite {
  test("PlantUML can render one component") {
    expect.eql(
      NonEmptyChain.of("@startuml", "", "component asdf", "", "@enduml"),
      PlantUml.render(
        NonEmptyChain.one(
          PlantUml.Component("asdf", None, None)
        )
      )
    )
  }

  test("PlantUML can render many components, AND lexicographically sorts them") {
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
}
