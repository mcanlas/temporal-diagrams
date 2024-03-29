package com.htmlism.temporaldiagrams.plantuml

import scala.util.chaining.*

import cats.data.*
import weaver.*

import com.htmlism.temporaldiagrams.*

object PlantUmlSuite extends FunSuite:
  test("Can render one component"):
    expect.eql(
      Chain("@startuml", "", "component asdf", "", "@enduml"),
      PlantUml.render(
        PlantUml.ComponentDiagram(
          PlantUml.Component("asdf", None, None)
        )
      )
    )

  test("Can render many components, AND lexicographically sorts them"):
    val xs =
      PlantUml.ComponentDiagram(
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
      PlantUml
        .ComponentDiagram(
          PlantUml.Link(
            "src",
            "dest"
          ),
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
      PlantUml
        .ComponentDiagram(
          PlantUml.Component("asdf", None, None),
          PlantUml.Component("asdf", None, None)
        )
        .pipe(PlantUml.render)
    )

  test("A component diagram can be built with varargs style or a foldable collection"):
    val xs =
      List(
        PlantUml.Component("foo", None, None),
        PlantUml.Component("bar", None, None)
      )

    val byVarargs =
      PlantUml
        .ComponentDiagram(xs*)

    val byFoldable =
      PlantUml.ComponentDiagram(xs)

    expect.same(byVarargs, byFoldable)
