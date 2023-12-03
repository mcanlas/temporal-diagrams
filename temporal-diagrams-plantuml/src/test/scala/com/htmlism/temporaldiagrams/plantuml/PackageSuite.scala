package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import weaver.*

import com.htmlism.temporaldiagrams.v2.DiagramEncoder

object PackageSuite extends FunSuite:
  test("A package sorts and indents its body"):
    expect.eql(
      Chain(
        "package outer {",
        "  component bar",
        "",
        "  component foo",
        "}"
      ),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Package("outer", PlantUml.Component("foo", None, None), PlantUml.Component("bar", None, None))
      )
    )
