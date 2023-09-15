package com.htmlism.temporaldiagrams.v2

import cats.data._
import cats.syntax.all._
import weaver._

object DiagramEncoderSuite extends FunSuite {
  test("A diagram encoder can encode") {
    expect.eql(
      NonEmptyList.one("component(abc)"),
      ToyDiagramLanguage.Component.componentEncoder.encode(ToyDiagramLanguage.Component("abc"))
    )
  }

  test("A diagram encoder is contravariant") {
    val stringEncoder =
      ToyDiagramLanguage
        .Component
        .componentEncoder
        .contramap((s: String) => ToyDiagramLanguage.Component(s"stringy $s"))

    expect.eql(NonEmptyList.one("component(stringy abc)"), stringEncoder.encode("abc"))
  }
}
