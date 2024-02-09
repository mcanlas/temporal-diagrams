package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import weaver.*

import com.htmlism.temporaldiagrams.DiagramEncoder

object TitleSuite extends FunSuite:
  test("A title supports multiple lines"):
    expect.eql(
      Chain(
        "title foo\nbar"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Title(List("foo", "bar"))
      )
    )

  test("An empty title renders nothing"):
    expect.eql(
      Chain.empty,
      DiagramEncoder[PlantUml].encode(
        PlantUml.Title(Nil)
      )
    )
