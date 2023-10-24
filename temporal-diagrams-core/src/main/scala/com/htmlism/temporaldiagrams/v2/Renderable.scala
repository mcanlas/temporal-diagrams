package com.htmlism.temporaldiagrams.v2

import scala.collection.immutable.ListSet
import scala.util.chaining.*

import cats.Monoid
import cats.data.Chain
import cats.syntax.all.*

/**
  * @tparam D
  *   The target diagram language
  */
sealed trait Renderable[D]:

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

object Renderable:

  /**
    * A trait to ease the hiding of the underlying domain type (shown in [[Renderable.OfA]], for cases where two
    * different domains are participating in the same diagram
    *
    * @tparam D
    *   The target diagram language
    */
  trait Of[D] extends Renderable[D]:

    /**
      * Returns a list of tags associated with this renderable object
      */
    def tags: ListSet[String]

  /**
    * @tparam D
    *   The target diagram language
    * @tparam A
    *   The source domain language
    */
  case class OfA[D, A](x: A, tags: ListSet[String])(using enc: HighlightEncoder[D, A]) extends Of[D]:
    def render: D =
      enc.encode(x)

    def renderWithHighlight(tag: String): D =
      enc.encodeWithHighlights(x, tags.contains(tag))

  // monoid for adt's that are multi arrow and don't have any dsl
  def renderMany[D: Monoid](xs: Chain[Renderable[D]]): D =
    xs
      .map(_.render)
      .fold

  // monoid for adt's that are multi arrow and don't have any dsl
  def renderManyWithTag[D: Monoid](xs: Chain[Renderable[D]], tag: String): D =
    xs
      .map(_.renderWithHighlight(tag))
      .fold

  def allTags(xs: Chain[Renderable[?]]): ListSet[String] =
    xs
      .iterator
      .flatMap { case x: Of[?] =>
        x.tags

//        case _ =>
//          ListSet.empty
      }
      .pipe(ListSet.from)
