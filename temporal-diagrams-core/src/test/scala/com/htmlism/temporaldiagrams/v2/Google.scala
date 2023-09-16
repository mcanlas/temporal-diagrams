package com.htmlism.temporaldiagrams.v2

import cats.data.NonEmptyList

import com.htmlism.temporaldiagrams.v2.ToyDiagramLanguage._

object Google {
  case class Compute(s: String)

  object Compute {
    implicit val computeEncoder: HighlightEncoder[NonEmptyList[ToyDiagramLanguage], Compute] =
      new HighlightEncoder[NonEmptyList[ToyDiagramLanguage], Compute] {
        def encode(x: Compute): NonEmptyList[ToyDiagramLanguage] =
          NonEmptyList.one(
            Component(s"google compute: ${x.s}")
          )

        def encodeWithHighlights(x: Compute, highlighted: Boolean): NonEmptyList[ToyDiagramLanguage] =
          NonEmptyList.one(
            Component(s"google compute: ${x.s} ${highlighted.toString}")
          )
      }
  }
}
