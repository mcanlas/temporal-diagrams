package com.htmlism.temporaldiagrams.v2

package object syntax {
  implicit class RenderableOps[D, A](x: A) {
    def r(implicit enc: HighlightEncoder[D, A]): Renderable[D] =
      RenderableA(x)
  }
}
