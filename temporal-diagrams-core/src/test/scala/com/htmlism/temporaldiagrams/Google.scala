package com.htmlism.temporaldiagrams

import cats.data.Chain

import com.htmlism.temporaldiagrams.ToyDiagramLanguage.*

object Google:
  case class Compute(s: String)

  object Compute:
    given HighlightEncoder[Chain[ToyDiagramLanguage], Compute] with
      def encode(x: Compute): Chain[ToyDiagramLanguage] =
        Chain(Component(s"google compute: ${x.s}"))

      def encodeWithHighlights(x: Compute, highlighted: Boolean): Chain[ToyDiagramLanguage] =
        Chain(Component(s"google compute: ${x.s} ${highlighted.toString}"))
