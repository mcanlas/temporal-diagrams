package com.htmlism.temporaldiagrams.v2

import cats.data.NonEmptyChain

import com.htmlism.temporaldiagrams.v2.ToyDiagramLanguage._

object Google {
  case class Compute(s: String)

  object Compute {
    // differs from amazon encoder, which is not nec
    implicit val computeNecEncoder: HighlightEncoder[NonEmptyChain[ToyDiagramLanguage], Compute] =
      new HighlightEncoder[NonEmptyChain[ToyDiagramLanguage], Compute] {
        def encode(x: Compute): NonEmptyChain[ToyDiagramLanguage] =
          NonEmptyChain.of(Component(s"google compute: ${x.s}"))

        def encodeWithHighlights(x: Compute, highlighted: Boolean): NonEmptyChain[ToyDiagramLanguage] =
          NonEmptyChain.of(Component(s"google compute: ${x.s} ${highlighted.toString}"))
      }
  }
}
