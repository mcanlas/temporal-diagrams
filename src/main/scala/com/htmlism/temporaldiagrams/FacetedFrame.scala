package com.htmlism.temporaldiagrams

/**
  * A renderable unit that responds to being keyed into a specific facet or version
  *
  * @tparam K
  *   Key type, generally `String`
  * @tparam A
  *   DSL type
  */
sealed trait FacetedFrame[K, A]

object FacetedFrame {
  type FrameId =
    String

  def from[K: Ordering, A](id: FrameId, x: (K, Renderable[A]), xs: (K, Renderable[A])*): FacetedFrame[K, A] =
    WithKeys(id, Nel(x, xs.toList))

  def fixed[K, A](x: Renderable[A]): FacetedFrame[K, A] =
    Fixed(x)

  case class WithKeys[K: Ordering, A](id: FrameId, xs: Nel[(K, Renderable[A])]) extends FacetedFrame[K, A] {
    def select(thatId: FrameId, thatK: K): FacetedFrame[K, A] =
      if (id == thatId)
        xs
          .find(kv => Ordering[K].equiv(kv._1, thatK))
          .map(_._2)
          .fold(this: FacetedFrame[K, A])(Fixed(_))
      else
        this
  }

  case class Fixed[K, A](x: Renderable[A]) extends FacetedFrame[K, A]

  /**
    * Given a collection of frames, fold in a collection of selectors (first one wins), refining them to be fixed. If
    * any unfixed remain, pick the default
    */
  def selectFrames[K, A](
      xs: Nel[FacetedFrame[K, A]],
      selectors: (FrameId, K)*
  ): Nel[Renderable[A]] =
    selectors
      .foldLeft(xs) { (xs, s) =>
        xs
          .map((refineKeysFrames[K, A] _).tupled(s))
      }
      .map(pickDefaults)

  private def refineKeysFrames[K, A](id: FrameId, k: K)(x: FacetedFrame[K, A]) =
    x match {
      case wk: WithKeys[K, A] =>
        wk.select(id, k)

      case _ =>
        x
    }

  private def pickDefaults[K, A](x: FacetedFrame[K, A]) =
    x match {
      case wk: WithKeys[K, A] =>
        wk.xs.head._2

      case Fixed(r) =>
        r
    }
}
