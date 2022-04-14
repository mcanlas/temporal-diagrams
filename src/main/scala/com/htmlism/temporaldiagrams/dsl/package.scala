package com.htmlism.temporaldiagrams

package object dsl {
  implicit class DslOps[A](x: A) {
    def r: Renderable[A] =
      ConstantRenderable(x)

    def id(id: String): Renderable[A] =
      IdentifiedRenderable(id, x)
  }

  implicit class RenderableOps[A](r: Renderable[A]) {
    def renderAs[B](implicit enc: DslEncoder[A, B]): String =
      enc.encode(r)

    def renderWithHighlightsOn[B](highlights: String*)(implicit enc: DslEncoder[A, B]): String =
      enc.encodeWithHighlights(r, highlights.toSet)
  }
}
