package com.htmlism.temporaldiagrams

package object syntax {
  implicit class ValueOps[A](x: A) {
    def iff(cond: Boolean, f: A => A): A =
      if (cond)
        f(x)
      else
        x

    def list: List[A] =
      List(x)

    def nel: Nel[A] =
      Nel.one(x)

    def r: Renderable[A] =
      Renderable.Tagged(Nil, x)

    def tag(tags: String*): Renderable[A] =
      Renderable.Tagged(tags.toList, x)
  }

  implicit class ValueOpsFaceted[A](x: Renderable[A]) {
    def f[K]: FacetedFrame[K, A] =
      FacetedFrame.fixed(x)
  }

  implicit class RenderableOps[A](r: Renderable[A]) {
    def encodeAs[B](implicit enc: DslEncoder[A, B]): List[B] =
      enc.encode(r)

    def encodeWithHighlightsOn[B](highlights: String*)(implicit enc: DslEncoder[A, B]): List[B] =
      enc.encodeWithHighlights(r, highlights.toSet)

    def renderAs[B](implicit B: Dialect[B], enc: DslEncoder[A, B]): String =
      B.consume(enc.encode(r), enc.injectedStyle)

    def renderWithHighlightsOn[B](highlights: String*)(implicit B: Dialect[B], enc: DslEncoder[A, B]): String =
      B.consume(enc.encodeWithHighlights(r, highlights.toSet), enc.injectedStyle)

    def keys: List[String] =
      Renderable.keys(r)
  }

  implicit class FacetedOps[K, A](xs: Nel[FacetedFrame[K, A]]) {
    def start: Narrative[K, A] =
      Narrative(xs, Nel.of(Nil))
  }
}
