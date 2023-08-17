package com.htmlism.temporaldiagrams.plantuml

import cats.data._
import weaver._

object PlantUmlSuite extends FunSuite {
  test("PlantUML can render one component") {
    expect.eql(
      NonEmptyList.of("@startuml", "", "@enduml"),
      PlantUml.render(Component("asdf", None))
    )
  }
}
