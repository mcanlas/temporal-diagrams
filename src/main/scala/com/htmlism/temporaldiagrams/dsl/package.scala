package com.htmlism.temporaldiagrams

package object dsl {
  implicit class ValueOps[A](x: A) {
    def r: Renderable[A] =
      Renderable.Anonymous(x)

    def id(id: String): Renderable[A] =
      Renderable.ById(id, x)
  }

  implicit class ValueOpsTemporal[A](x: Renderable[A]) {
    def t[K]: Temporal[K, A] =
      Temporal.FixedTemporal(x)
  }

  implicit class RenderableOps[A](r: Renderable[A]) {
    def renderAs[B](implicit enc: DslEncoder[A, B]): String =
      enc.encode(r)

    def renderWithHighlightsOn[B](highlights: String*)(implicit enc: DslEncoder[A, B]): String =
      enc.encodeWithHighlights(r, highlights.toSet)
  }

  implicit class TemporalOps[K: Ordering, A](t: Temporal[K, A]) {
    def at(k: K): Renderable[A] =
      Temporal
        .resolve(t, k)
  }
}
