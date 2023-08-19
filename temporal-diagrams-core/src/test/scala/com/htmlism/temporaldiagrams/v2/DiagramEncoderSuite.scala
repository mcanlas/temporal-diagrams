package com.htmlism.temporaldiagrams.v2

import cats.data._
import cats.syntax.all._
import weaver._

object DiagramEncoderSuite extends FunSuite {
  trait TestDsl

  private val intEncoder = new DiagramEncoder[TestDsl, Int] {
    def encode(x: Int): NonEmptyList[String] =
      NonEmptyList.one(x.toString)
  }

  test("A diagram encoder can encode") {
    expect.eql(NonEmptyList.one("123"), intEncoder.encode(123))
  }

  test("A diagram encoder is contravariant") {
    val stringEncoder =
      intEncoder.contramap((s: String) => s.length)

    expect.eql(NonEmptyList.one("5"), stringEncoder.encode("apple"))
  }
}
