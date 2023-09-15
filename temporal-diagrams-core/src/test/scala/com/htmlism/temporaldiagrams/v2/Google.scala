package com.htmlism.temporaldiagrams.v2

import com.htmlism.temporaldiagrams.v2.ToyDiagramLanguage._

object Google {
  case class Compute(s: String)

  object Compute {
    implicit val computeEncoder: HighlightEncoder[Component, Compute] =
      new HighlightEncoder[Component, Compute] {
        def encode(x: Compute): Component =
          Component(s"google compute: ${x.s}")

        def encodeWithHighlights(x: Compute, highlighted: Boolean): Component =
          Component(s"google compute: ${x.s} ${highlighted.toString}")
      }
  }
}
