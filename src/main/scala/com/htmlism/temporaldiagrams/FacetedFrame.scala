package com.htmlism.temporaldiagrams

import cats.data._

/**
 * A renderable unit that responds to being keyed into a specific facet or version
 *
 * @tparam K Key type, generally `String`
 * @tparam A DSL type
 */
sealed trait FacetedFrame[K, A]

object FacetedFrame {
  def apply[K, A](id: String, x: (K, Renderable[A]), xs: (K, Renderable[A])*): FacetedFrame[K, A] =
    WithKeys(id, NonEmptyList(x, xs.toList))

  case class WithKeys[K, A](id: String, xs: NonEmptyList[(K, Renderable[A])]) extends FacetedFrame[K, A]

  case class Fixed[K, A](x: Renderable[A]) extends FacetedFrame[K, A]
}
