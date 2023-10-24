package com.htmlism.temporaldiagrams.v2

import cats.data.*
import cats.syntax.all.*
import weaver.*

object DiagramEncoderSuite extends FunSuite:
  test("A diagram encoder can encode"):
    expect.eql(
      Chain("component(abc)"),
      summon[DiagramEncoder[ToyDiagramLanguage]].encode(ToyDiagramLanguage.Component("abc"))
    )

  test("A diagram encoder is contravariant"):
    val stringEncoder =
      summon[DiagramEncoder[ToyDiagramLanguage]]
        .contramap((s: String) => ToyDiagramLanguage.Component(s"stringy $s"))

    expect.eql(Chain("component(stringy abc)"), stringEncoder.encode("abc"))
