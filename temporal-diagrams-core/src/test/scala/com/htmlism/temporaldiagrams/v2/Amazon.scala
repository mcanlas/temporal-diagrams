package com.htmlism.temporaldiagrams.v2

import cats.data.NonEmptyList

import com.htmlism.temporaldiagrams.v2.ToyDiagramLanguage._

object Amazon {
  case class Ec2(s: String)

  object Ec2 {
    implicit val ec2Encoder: HighlightEncoder[NonEmptyList[ToyDiagramLanguage], Ec2] =
      new HighlightEncoder[NonEmptyList[ToyDiagramLanguage], Ec2] {
        def encode(x: Ec2): NonEmptyList[ToyDiagramLanguage] =
          NonEmptyList.one(
            Component(s"amazon ec2: ${x.s}")
          )

        def encodeWithHighlights(x: Ec2, highlighted: Boolean): NonEmptyList[ToyDiagramLanguage] =
          NonEmptyList.one(
            Component(s"amazon ec2: ${x.s} ${highlighted.toString}")
          )
      }
  }
}
