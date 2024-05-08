package com.htmlism.temporaldiagrams

import cats.data.*
import cats.syntax.all.*
import weaver.*

import com.htmlism.temporaldiagrams.syntax.*

object DiagramEncoderSuite extends FunSuite:
  test("A diagram encoder can encode"):
    expect.eql(
      Chain("component(abc)"),
      Chain
        .one[ToyDiagramLanguage]:
          ToyDiagramLanguage.Component("abc")
        .encode
    )

  test("A diagram encoder is contravariant"):
    given DiagramEncoder[String] =
      summon[DiagramEncoder[Chain[ToyDiagramLanguage]]]
        .contramap((s: String) =>
          Chain:
            ToyDiagramLanguage.Component(s"stringy $s")
        )

    expect.eql(Chain("component(stringy abc)"), "abc".encode)
