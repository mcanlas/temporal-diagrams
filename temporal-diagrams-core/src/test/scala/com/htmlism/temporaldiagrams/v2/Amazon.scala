package com.htmlism.temporaldiagrams.v2

object Amazon {
  case class Ec2(s: String)

  object Ec2 {
    implicit val ec2Encoder: HighlightEncoder[String, Ec2] =
      new HighlightEncoder[String, Ec2] {
        def encode(x: Ec2): String =
          s"amazon ec2 ${x.s}"

        def encodeWithHighlights(x: Ec2, highlighted: Boolean): String =
          s"amazon ec2 ${x.s} ${highlighted.toString}"
      }
  }
}
