package com.htmlism.temporaldiagrams.plantuml

import cats.data._
import weaver._

object PlantUmlSuite extends FunSuite {
  test("PlantUML can render one component") {
    expect.eql(
      NonEmptyList.of("@startuml", "component asdf", "@enduml"),
      PlantUml.render(Component("asdf", None))
    )
  }

  test("PlantUML can render many components") {
    val xs =
      NonEmptyList
        .of(
          Component("foo", None),
          Component("bar", None)
        )

    expect.eql(
      NonEmptyList.of("@startuml", "component foo", "component bar", "@enduml"),
      PlantUml.render(xs)
    )
  }
}
