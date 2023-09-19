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
    * Renders this object into target language `D` for a specific highlight tag
    *
    * @param tag
    *   If the renderable has this tag, it will be rendered with its highlighted style; otherwise it will use its dim
    *   style
    */
  def renderWithHighlight(tag: String): D

  /**
    * Returns a list of tags associated with this renderable object
    */
  def tags: List[String]
}

object Renderable {
  case class Of[D, A](x: A, tags: List[String])(implicit enc: HighlightEncoder[D, A]) extends Renderable[D] {
    def render: D =
      enc.encode(x)

    def renderWithHighlight(tag: String): D =
      enc.encodeWithHighlights(x, tags.contains(tag))
  }

  def renderMany[D: Semigroup](xs: NonEmptyList[Renderable[D]]): D =
    xs
      .map(_.render)
      .reduce

  def renderManyWithTag[D: Semigroup](xs: NonEmptyList[Renderable[D]], tag: String): D =
    xs
      .map(_.renderWithHighlight(tag))
      .reduce

  def allTags(xs: NonEmptyList[Renderable[_]]): List[String] =
    xs
      .toList
      .flatMap(_.tags)
      .distinct
}
