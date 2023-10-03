package com.htmlism.temporaldiagrams.v2

import com.htmlism.temporaldiagrams.v2.ToyDiagramLanguage.*

object Amazon {
  case class Ec2(s: String)

  object Ec2 {
    implicit val ec2Encoder: HighlightEncoder[ToyDiagramLanguage, Ec2] =
      new HighlightEncoder[ToyDiagramLanguage, Ec2] {
        def encode(x: Ec2): ToyDiagramLanguage =
          Component(s"amazon ec2: ${x.s}")

        def encodeWithHighlights(x: Ec2, highlighted: Boolean): ToyDiagramLanguage =
          Component(s"amazon ec2: ${x.s} ${highlighted.toString}")
      }
  }
}
