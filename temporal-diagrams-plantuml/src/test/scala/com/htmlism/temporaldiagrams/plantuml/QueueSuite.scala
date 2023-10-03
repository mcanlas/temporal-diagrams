package com.htmlism.temporaldiagrams.plantuml

import cats.data._
import cats.syntax.all._
import weaver._

import com.htmlism.temporaldiagrams.v2.DiagramEncoder

object QueueSuite extends FunSuite {
  test("A queue has an name") {
    expect.eql(
      NonEmptyChain.one("queue asdf"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Queue("asdf", None, None)
      )
    )
  }

  test("A queue has an optional override alias") {
    expect.eql(
      NonEmptyChain.one("queue foo as bar"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Queue("foo", "bar".some, None)
      )
    )
  }
}
