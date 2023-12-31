package com.htmlism.temporaldiagrams

import cats.data.NonEmptyList

/**
  * A renderable unit that responds to being keyed into a specific facet or version
  *
  * @tparam K
  *   Key type, generally `String`
  * @tparam A
  *   DSL type
  */
sealed trait FacetedFrame[K, +A]

object FacetedFrame:
  def from[K: Ordering, A](
      frameId: String,
      x: (K, List[Renderable.Tagged[A]]),
      xs: (K, List[Renderable.Tagged[A]])*
  ): FacetedFrame[K, A] =
    WithKeys(frameId, NonEmptyList(x, xs.toList))

  def fixed[K, A](x: List[Renderable.Tagged[A]]): FacetedFrame[K, A] =
    Fixed(x)

  case class WithKeys[K: Ordering, +A](frameId: String, xs: NonEmptyList[(K, List[Renderable.Tagged[A]])])
      extends FacetedFrame[K, A]:
    def select(thatId: String, thatK: K): FacetedFrame[K, A] =
      if frameId == thatId then
        xs
          .find(kv => Ordering[K].equiv(kv._1, thatK))
          .map(_._2)
          .fold(this: FacetedFrame[K, A])(Fixed(_))
      else this

  case class Fixed[K, +A](x: List[Renderable.Tagged[A]]) extends FacetedFrame[K, A]

  /**
    * Given a collection of frames, fold in a collection of selectors (first one wins), refining them to be fixed. If
    * any unfixed remain, pick the default
    */
  def selectFrames[K, A](
      xs: NonEmptyList[FacetedFrame[K, A]],
      selectors: (String, K)*
  ): List[Renderable.Tagged[A]] =
    selectors
      .foldLeft(xs) { (xs, s) =>
        xs
          .map(refineKeysFrames[K, A].tupled(s))
      }
      .toList
      .flatMap(pickDefaults)

  private def refineKeysFrames[K, A](id: String, k: K)(x: FacetedFrame[K, A]) =
    x match
      case wk: WithKeys[K, A] =>
        wk.select(id, k)

      case _ =>
        x

  private def pickDefaults[K, A](x: FacetedFrame[K, A]) =
    x match
      case wk: WithKeys[K, A] =>
        wk.xs.head._2

      case Fixed(r) =>
        r
