package com.htmlism.temporaldiagrams.v2

import cats.data.NonEmptyList

import com.htmlism.temporaldiagrams.v2.ToyDiagramLanguage._

object Google {
  case class Compute(s: String)

  object Compute {
    // differs from amazon encoder, which is not nel
    implicit val computeNelEncoder: HighlightEncoder[NonEmptyList[ToyDiagramLanguage], Compute] =
      new HighlightEncoder[NonEmptyList[ToyDiagramLanguage], Compute] {
        def encode(x: Compute): NonEmptyList[ToyDiagramLanguage] =
          NonEmptyList.of(Component(s"google compute: ${x.s}"))

        def encodeWithHighlights(x: Compute, highlighted: Boolean): NonEmptyList[ToyDiagramLanguage] =
          NonEmptyList.of(Component(s"google compute: ${x.s} ${highlighted.toString}"))
      }
  }
}
