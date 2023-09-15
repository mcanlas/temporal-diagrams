package com.htmlism.temporaldiagrams.v2

package object syntax {
  implicit def bindHighlightEncoder[D, A](x: A)(implicit enc: HighlightEncoder[D, A]): Renderable.One[D, A] =
    Renderable.One(x)
}
