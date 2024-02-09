package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import weaver.*

import com.htmlism.temporaldiagrams.DiagramEncoder

object LegendSuite extends FunSuite:
  test("A legend supports multiple lines"):
    expect.eql(
      Chain(
        "legend",
        "foo",
        "bar",
        "endlegend"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Legend(List("foo", "bar"))
      )
    )

  test("An empty legend renders nothing"):
    expect.eql(
      Chain.empty,
      DiagramEncoder[PlantUml].encode(
        PlantUml.Legend(Nil)
      )
    )
