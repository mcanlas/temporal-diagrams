package com.htmlism.temporaldiagrams.v2

import com.htmlism.temporaldiagrams.v2.ToyDiagramLanguage._

object Google {
  case class Compute(s: String)

  object Compute {
    implicit val computeEncoder: HighlightEncoder[ToyDiagramLanguage, Compute] =
      new HighlightEncoder[ToyDiagramLanguage, Compute] {
        def encode(x: Compute): ToyDiagramLanguage =
          Component(s"google compute: ${x.s}")

        def encodeWithHighlights(x: Compute, highlighted: Boolean): ToyDiagramLanguage =
          Component(s"google compute: ${x.s} ${highlighted.toString}")
      }
  }
}
