package com.htmlism.temporaldiagrams.v2

import com.htmlism.temporaldiagrams.v2.ToyDiagramLanguage._

object Amazon {
  case class Ec2(s: String)

  object Ec2 {
    implicit val ec2Encoder: HighlightEncoder[Component, Ec2] =
      new HighlightEncoder[Component, Ec2] {
        def encode(x: Ec2): Component =
          Component(s"amazon ec2: ${x.s}")

        def encodeWithHighlights(x: Ec2, highlighted: Boolean): Component =
          Component(s"amazon ec2: ${x.s} ${highlighted.toString}")
      }
  }
}
