package com.htmlism.temporaldiagrams.plantuml

import scala.util.chaining.*

import cats.data.*
import weaver.*

import com.htmlism.temporaldiagrams.v2.*

object PlantUmlSuite extends FunSuite:
  test("Can render one component"):
    expect.eql(
      Chain("@startuml", "", "component asdf", "", "@enduml"),
      PlantUml.render(
        Chain(
          PlantUml.Component("asdf", None, None)
        )
      )
    )

  test("Can render horizontally"):
    expect.eql(
      Chain("@startuml", "", "left to right direction", "", "component asdf", "", "@enduml"),
      PlantUml.renderHorizontally(
        Chain(
          PlantUml.Component("asdf", None, None)
        )
      )
    )

  test("Can render many components, AND lexicographically sorts them"):
    val xs =
      Chain[PlantUml](
        PlantUml.Component("foo", None, None),
        PlantUml.Component("bar", None, None)
      )

    expect.eql(
      Chain("@startuml", "", "component bar", "", "component foo", "", "@enduml"),
      PlantUml.render(xs)
    )

  test("Can render the left to right directive"):
    expect.eql(
      Chain("left to right direction"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.LeftToRightDirection
      )
    )

  test("Can derive a non-empty chain highlight encoder from an elemental highlight encoder"):
    given HighlightEncoder[PlantUml, NecTestDsl] =
      new HighlightEncoder[PlantUml, NecTestDsl]:
        def encode(x: NecTestDsl): PlantUml =
          PlantUml.Component(x.s, None, None)

        def encodeWithHighlights(x: NecTestDsl, highlighted: Boolean): PlantUml =
          PlantUml.Component(s"${x.s} with highlights", None, None)

    val derivedEncoder =
      summon[HighlightEncoder[Chain[PlantUml], NecTestDsl]]

    val x =
      NecTestDsl("asdf")

    expect.eql(
      Chain(PlantUml.Component("asdf", None, None)),
      derivedEncoder.encode(x)
    ) and expect.eql(
      Chain(PlantUml.Component("asdf with highlights", None, None)),
      derivedEncoder.encodeWithHighlights(x, highlighted = true)
    )

  case class NecTestDsl(s: String)

  test("Components are rendered in an order and lexicographically"):
    expect.eql(
      Chain(
        "@startuml",
        "",
        "left to right direction",
        "",
        "skinparam foo {",
        "}",
        "",
        "component asdf",
        "",
        "src --> dest",
        "",
        "@enduml"
      ),
      Chain(
        PlantUml.Arrow("src", "dest", None),
        PlantUml.Component("asdf", None, None),
        PlantUml.SkinParamGroup("foo"),
        PlantUml.LeftToRightDirection
      )
        .pipe(PlantUml.render)
    )

  test("Duplicate components are rendered uniquely"):
    expect.eql(
      Chain(
        "@startuml",
        "",
        "component asdf",
        "",
        "@enduml"
      ),
      Chain(
        PlantUml.Component("asdf", None, None),
        PlantUml.Component("asdf", None, None)
      )
        .pipe(PlantUml.render)
    )
