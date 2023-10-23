package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import weaver.*

import com.htmlism.temporaldiagrams.v2.DiagramEncoder

object SkinParamSuite extends FunSuite:
  test("Can render skin params"):
    expect.eql(
      NonEmptyChain.of(
        "skinparam asdf {",
        "}"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.SkinParamGroup("asdf")
      )
    )

  test("Has a builder method"):
    expect.eql(
      NonEmptyChain.of(
        "skinparam asdf {",
        "  key value",
        "}"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml
          .SkinParamGroup("asdf")
          .and("key", "value")
      )
    )

  test("A skin param has an optional stereotype"):
    expect.eql(
      NonEmptyChain.of(
        "skinparam asdf<< stereo >> {",
        "}"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.SkinParamGroup("asdf", "stereo")
      )
    )
