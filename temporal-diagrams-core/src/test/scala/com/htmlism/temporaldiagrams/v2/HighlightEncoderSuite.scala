package com.htmlism.temporaldiagrams.v2

import cats.syntax.all._
import weaver._

object HighlightEncoderSuite extends FunSuite {
  case class TestDomainObject(s: String)

  private val strEncoder = new HighlightEncoder[String, TestDomainObject] {
    def encode(x: TestDomainObject): String =
      x.s

    def encodeWithHighlights(x: TestDomainObject, highlighted: Boolean): String =
      x.s + highlighted.toString
  }

  test("A diagram encoder can encode") {
    expect.eql("abc", strEncoder.encode(TestDomainObject("abc")))
  }

  test("A diagram encoder can encode with highlights") {
    expect.eql(
      "abctrue",
      strEncoder.encodeWithHighlights(
        TestDomainObject("abc"),
        highlighted = true
      )
    )
  }

  test("A diagram encoder is contravariant") {
    val repeatEncoder =
      strEncoder.contramap((s: String) => TestDomainObject(s + s))

    expect.eql("appleapple", repeatEncoder.encode("apple"))
  }
}
