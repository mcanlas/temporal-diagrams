package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import weaver.*

import com.htmlism.temporaldiagrams.v2.DiagramEncoder

object CaptionSuite extends FunSuite:
  test("A caption supports multiple lines"):
    expect.eql(
      Chain(
        "caption foo\nbar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Caption(List("foo", "bar"))
      )
    )

  test("An empty caption renders nothing"):
    expect.eql(
      Chain.empty,
      DiagramEncoder[PlantUml].encode(
        PlantUml.Caption(Nil)
      )
    )
