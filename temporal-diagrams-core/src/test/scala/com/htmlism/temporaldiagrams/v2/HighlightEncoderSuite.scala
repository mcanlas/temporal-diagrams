package com.htmlism.temporaldiagrams.v2

import weaver._

object HighlightEncoderSuite extends FunSuite {
  case class TestDomainDsl(s: String)

  private val strEncoder = new HighlightEncoder[TestDomainDsl, String] {
    def encode(x: TestDomainDsl): String =
      x.s

    def encodeWithHighlights(x: TestDomainDsl, highlighted: Boolean): String =
      x.s + highlighted.toString
  }

  test("A diagram encoder can encode") {
    expect.eql("abc", strEncoder.encode(TestDomainDsl("abc")))
  }

  // TODO encode with highlights

  // TODO contramap
}
