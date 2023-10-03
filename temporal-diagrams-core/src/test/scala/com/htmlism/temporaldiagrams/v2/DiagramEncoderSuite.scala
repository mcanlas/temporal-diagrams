package com.htmlism.temporaldiagrams.v2

import cats.data.*
import cats.syntax.all.*
import weaver.*

object DiagramEncoderSuite extends FunSuite {
  test("A diagram encoder can encode") {
    expect.eql(
      NonEmptyChain.one("component(abc)"),
      ToyDiagramLanguage.toyEncoder.encode(ToyDiagramLanguage.Component("abc"))
    )
  }

  test("A diagram encoder is contravariant") {
    val stringEncoder =
      ToyDiagramLanguage
        .toyEncoder
        .contramap((s: String) => ToyDiagramLanguage.Component(s"stringy $s"))

    expect.eql(NonEmptyChain.one("component(stringy abc)"), stringEncoder.encode("abc"))
  }
}
