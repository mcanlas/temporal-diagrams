package com.htmlism.temporaldiagrams.v2

import scala.collection.immutable.ListSet
import scala.util.chaining.*

import cats.Semigroup
import cats.data.NonEmptyChain

/**
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
  def tags: ListSet[String]
}

object Renderable {

  /**
    * A trait to ease the hiding of the underlying domain type (shown in [[Renderable.OfA]], for cases where two
    * different domains are participating in the same diagram
    *
    * @tparam D
    *   The target diagram language
    */
  trait Of[D] extends Renderable[D]

  /**
    * @tparam D
    *   The target diagram language
    * @tparam A
    *   The source domain language
    */
  case class OfA[D, A](x: A, tags: ListSet[String])(implicit enc: HighlightEncoder[D, A]) extends Of[D] {
    def render: D =
      enc.encode(x)

    def renderWithHighlight(tag: String): D =
      enc.encodeWithHighlights(x, tags.contains(tag))
  }

  def renderMany[D: Semigroup](xs: NonEmptyChain[Renderable[D]]): D =
    xs
      .map(_.render)
      .reduce

  def renderManyWithTag[D: Semigroup](xs: NonEmptyChain[Renderable[D]], tag: String): D =
    xs
      .map(_.renderWithHighlight(tag))
      .reduce

  def allTags(xs: NonEmptyChain[Renderable[?]]): ListSet[String] =
    xs
      .iterator
      .flatMap(_.tags)
      .pipe(ListSet.from)
}
