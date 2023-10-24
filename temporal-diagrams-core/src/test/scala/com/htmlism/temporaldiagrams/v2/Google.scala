package com.htmlism.temporaldiagrams.v2

import cats.data.Chain

import com.htmlism.temporaldiagrams.v2.ToyDiagramLanguage.*

object Google:
  case class Compute(s: String)

  object Compute:
    // differs from amazon encoder, which is not nec
    given HighlightEncoder[Chain[ToyDiagramLanguage], Compute] =
      new HighlightEncoder[Chain[ToyDiagramLanguage], Compute]:
        def encode(x: Compute): Chain[ToyDiagramLanguage] =
          Chain(Component(s"google compute: ${x.s}"))

        def encodeWithHighlights(x: Compute, highlighted: Boolean): Chain[ToyDiagramLanguage] =
          Chain(Component(s"google compute: ${x.s} ${highlighted.toString}"))
