package com.htmlism.temporaldiagrams.v2

import cats.data.*
import cats.syntax.all.*
import weaver.*

object DiagramEncoderSuite extends FunSuite:
  test("A diagram encoder can encode"):
    expect.eql(
      Chain("component(abc)"),
      summon[DiagramEncoder[Chain[ToyDiagramLanguage]]].encode:
        Chain:
          ToyDiagramLanguage.Component("abc")
    )

  test("A diagram encoder is contravariant"):
    val stringEncoder =
      summon[DiagramEncoder[Chain[ToyDiagramLanguage]]]
        .contramap((s: String) =>
          Chain:
            ToyDiagramLanguage.Component(s"stringy $s")
        )

    expect.eql(Chain("component(stringy abc)"), stringEncoder.encode("abc"))
