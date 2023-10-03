package com.htmlism.temporaldiagrams.plantuml

import cats.data._
import cats.syntax.all._
import weaver._

import com.htmlism.temporaldiagrams.v2.DiagramEncoder

object DatabaseSuite extends FunSuite {
  test("A database has an name") {
    expect.eql(
      NonEmptyChain.one("database asdf"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Database("asdf", None, None)
      )
    )
  }

  test("A database has an optional override alias") {
    expect.eql(
      NonEmptyChain.one("database foo as bar"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Database("foo", "bar".some, None)
      )
    )
  }
}
