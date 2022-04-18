package com.htmlism.temporaldiagrams

package object syntax {
  implicit class ValueOps[A](x: A) {
    def r: Renderable[A] =
      Renderable.Anonymous(x)

    def id(id: String): Renderable[A] =
      Renderable.ById(id, x)
  }

  implicit class ValueOpsTemporal[A](x: Renderable[A]) {
    def t[K]: TemporalFrame[K, A] =
      TemporalFrame.Fixed(x)
  }

  implicit class RenderableOps[A](r: Renderable[A]) {
    def encodeAs[B](implicit enc: DslEncoder[A, B]): List[B] =
      enc.encode(r)

    def encodeWithHighlightsOn[B](highlights: String*)(implicit enc: DslEncoder[A, B]): List[B] =
      enc.encodeWithHighlights(r, highlights.toSet)

    def renderAs[B](implicit B: Dialect[B], enc: DslEncoder[A, B]): String =
      B.consume(enc.encode(r), hasHighlights = false)

    def renderWithHighlightsOn[B](highlights: String*)(implicit B: Dialect[B], enc: DslEncoder[A, B]): String =
      B.consume(enc.encodeWithHighlights(r, highlights.toSet), hasHighlights = true)

    def keys: List[String] =
      Renderable.keys(r)
  }

  implicit class TemporalOps[K: Ordering, A](t: TemporalFrame[K, A]) {
    def at(k: K): Renderable[A] =
      TemporalFrame
        .resolve(t, k)

    def keys: List[K] =
      TemporalFrame
        .keys(t)
  }
}
