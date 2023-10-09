package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import com.htmlism.temporaldiagrams.v2.DiagramEncoder
import weaver.*

object SkinParamSuite extends FunSuite {
  test("A skin param has an optional stereotype") {
    expect.eql(
      NonEmptyChain.of(
        "skinparam asdf<< stereo >> {",
        "}"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.SkinParamGroup("asdf", "stereo")
      )
    )
  }
}
