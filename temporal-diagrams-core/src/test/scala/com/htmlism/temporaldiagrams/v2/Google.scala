package com.htmlism.temporaldiagrams.v2

object Google {
  case class Compute(s: String)

  object Compute {
    implicit val computeEncoder: HighlightEncoder[String, Compute] =
      new HighlightEncoder[String, Compute] {
        def encode(x: Compute): String =
          s"google compute: ${x.s}"

        def encodeWithHighlights(x: Compute, highlighted: Boolean): String =
          s"google compute: ${x.s} ${highlighted.toString}"
      }
  }
}
