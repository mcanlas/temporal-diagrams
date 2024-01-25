package com.htmlism.temporaldiagrams.v2

import scala.collection.immutable.ListSet
import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.syntax.all.*

type Renderable[A] =
  Renderable.Of[A]

object Renderable:
  /**
    * @tparam D
    *   The target diagram language
    * @tparam K
    *   The identifier type for multi arrow sources and destinations
    */
  sealed trait WithMultiArrows[+D, +K]

  /**
    * A trait to ease the hiding of the underlying domain type (shown in [[Renderable.OfA]], for cases where two
    * different domains are participating in the same diagram
    *
    * @tparam D
    *   The target diagram language
    * @tparam K
    *   The type of underlying source and destination arguments to multi arrows
    */

  object WithMultiArrows:
    case class Source[K](alias: String, sources: List[K]) extends WithMultiArrows[Nothing, K]

    case class Destination[K](alias: String, destinations: List[K]) extends WithMultiArrows[Nothing, K]

    // TODO support tags
    case class MultiArrow(sourceAlias: String, destinationAlias: String) extends WithMultiArrows[Nothing, Nothing]

    def renderArrows[D, A, K: Eq](xs: Chain[Renderable.WithMultiArrows[D, A]])(using A: MultiArrowEncoder[K, A])(using
        HighlightEncoder[D, A]
    ): ValidatedNec[String, Chain[Renderable[D]]] =
      val (sources, destinations, specs, renderables) =
        xs
          .map:
            case src: Source[?] =>
              (Chain(src.asInstanceOf[Source[K]]), Chain.empty, Chain.empty, Chain.empty)
            case dest: Destination[?] =>
              (Chain.empty, Chain(dest.asInstanceOf[Destination[K]]), Chain.empty, Chain.empty)
            case arrow: MultiArrow =>
              (Chain.empty, Chain.empty, Chain(arrow), Chain.empty)
            case x: Renderable[?] =>
              (Chain.empty, Chain.empty, Chain.empty, Chain(x.asInstanceOf[Renderable[D]]))
          .combineAll

      val arrowRenderables = specs
        .flatTraverse: s =>
          val vSrc =
            Validated.fromOption(
              sources.find(_.alias == s.sourceAlias),
              NonEmptyChain.one:
                s"specified source alias ${s.sourceAlias} was not defined"
            )

          val vDest =
            Validated.fromOption(
              destinations.find(_.alias == s.destinationAlias),
              NonEmptyChain.one:
                s"specified destination alias ${s.destinationAlias} was not defined"
            )

          (vSrc, vDest)
            .mapN: (mSrc, mDest) =>
              Chain
                .fromSeq:
                  for
                    src  <- mSrc.sources
                    dest <- mDest.destinations
                  yield Renderable.OfA(A.encodeArrow(src, dest), ListSet.empty): Renderable.Of[D]

      arrowRenderables
        .map(_ |+| renderables)

    def dropArrows[D: Monoid, A](xs: Chain[Renderable.WithMultiArrows[D, A]]): Chain[Renderable[D]] =
      xs
        .collect:
          case x: Renderable[?] => x.asInstanceOf[Renderable[D]]

  sealed trait Taggable:
    /**
      * Returns a list of tags associated with this renderable object
      */
    def tags: ListSet[String]

  sealed trait Of[D] extends Renderable.WithMultiArrows[D, Nothing] with Taggable:

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
    * @tparam D
    *   The target diagram language
    * @tparam A
    *   The source domain language
    */
  case class OfA[A, D](x: A, tags: ListSet[String])(using enc: HighlightEncoder[D, A]) extends Of[D]:
    def render: D =
      enc.encode(x)

    def renderWithHighlight(tag: String): D =
      enc.encodeWithHighlights(x, tags.contains(tag))

    // TODO test
    def withTag(tag: String): OfA[A, D] =
      copy(tags = tags + tag)

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
