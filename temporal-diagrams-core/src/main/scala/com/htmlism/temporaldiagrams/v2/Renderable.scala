package com.htmlism.temporaldiagrams.v2

import cats.Semigroup
import cats.data.NonEmptyList

/**
  * A trait to make the hiding of the domain type easier, for cases where two different domains are participating in the
  * same diagram
  *
  * @tparam D
  *   The target diagram language
  */
sealed trait Renderable[D] {

  /**
    * Renders this object into target language `D`
    */
  def render: D

  /**
    * Returns a list of tags associated with this renderable object
    */
  def tags: List[String]
}

case class RenderableA[D, A](x: A, tags: List[String])(implicit enc: HighlightEncoder[D, A]) extends Renderable[D] {
  def render: D =
    enc.encode(x)
}

object Renderable {
  def renderMany[D: Semigroup](xs: NonEmptyList[Renderable[D]]): D =
    xs
      .map(_.render)
      .reduce

}
