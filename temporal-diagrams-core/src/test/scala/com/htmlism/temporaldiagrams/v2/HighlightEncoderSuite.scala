package com.htmlism.temporaldiagrams.v2

import cats.syntax.all._
import weaver._

import com.htmlism.temporaldiagrams.v2.ToyDiagramLanguage._

object HighlightEncoderSuite extends FunSuite {
  test("A diagram encoder can encode") {
    expect.eql(Component("amazon ec2: abc"), Amazon.Ec2.ec2Encoder.encode(Amazon.Ec2("abc")))
  }

  test("A diagram encoder can encode with highlights") {
    expect.eql(
      Component("amazon ec2: abc true"),
      Amazon
        .Ec2
        .ec2Encoder
        .encodeWithHighlights(
          Amazon.Ec2("abc"),
          highlighted = true
        )
    )
  }

  test("A diagram encoder is contravariant") {
    val repeatEncoder =
      Amazon.Ec2.ec2Encoder.contramap((s: String) => Amazon.Ec2(s"$s $s"))

    expect.eql(Component("amazon ec2: twin twin"), repeatEncoder.encode("twin"))
  }
}
