package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import com.htmlism.temporaldiagrams.v2.*
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

  test("Can derive a non-empty chain highlight encoder from an elemental highlight encoder") {
    implicit val elementEncoder: HighlightEncoder[PlantUml, FooDsl] = {
      new HighlightEncoder[PlantUml, FooDsl] {
        def encode(x: FooDsl): PlantUml =
          PlantUml.Component(x.s, None, None)

        def encodeWithHighlights(x: FooDsl, highlighted: Boolean): PlantUml =
          PlantUml.Component(s"${x.s} with highlights", None, None)
      }
    }

    val derivedEncoder =
      implicitly[HighlightEncoder[NonEmptyChain[PlantUml], FooDsl]]

    val x =
      FooDsl("asdf")

    expect.eql(
      NonEmptyChain.one(PlantUml.Component("asdf", None, None)),
      derivedEncoder.encode(x)
    ) and expect.eql(
      NonEmptyChain.one(PlantUml.Component("asdf with highlights", None, None)),
      derivedEncoder.encodeWithHighlights(x, highlighted = true)
    )
  }

  case class FooDsl(s: String)
}
