package com.htmlism.temporaldiagrams

import cats.data.NonEmptyList
import cats.syntax.all.*

/**
  * Describes an encoding bridge from DSL `A` to diagram dialect `B`
  */
trait DslEncoder[A, B]:
  def encode(x: A): List[B]

  def encodeWithHighlights(x: A, highlighted: Boolean): List[B]

  /**
    * This convenience method is used by the host language `A` as an opportunity to render arrows given only their
    * source and destination aliases.
    */
  protected def renderArrow(src: String, dest: String): List[Renderable.Tagged[A]]

  /**
    * Encoders opt-in to showing debug strings somewhere in the diagrams by implementing this method
    */
  protected def debug(xs: List[String]): List[B]

object DslEncoder:
  def encodeMany[A, B](xs: List[Renderable[A]])(using ev: DslEncoder[A, B]): List[B] =
    encodeCommon(xs)(
      _.map(_.x)
        .flatMap(ev.encode)
    )

  def encodeManyWithHighlights[A, B](xs: List[Renderable[A]], highlights: String*)(using
      ev: DslEncoder[A, B]
  ): List[B] =
    encodeCommon(xs)(_.flatMap { case Renderable.Tagged(tags, x) =>
      if (highlights intersect tags).nonEmpty then ev.encodeWithHighlights(x, highlighted = true)
      else ev.encodeWithHighlights(x, highlighted                                         = false)
    })

  private def encodeCommon[A, B](xs: List[Renderable[A]])(f: List[Renderable.Tagged[A]] => List[B])(using
      ev: DslEncoder[A, B]
  ): List[B] =
    val srcLookup =
      xs.collect { case Renderable.Source(k, vs) => Map(k -> vs) }
        .foldLeft(Map.empty[String, NonEmptyList[String]])(_ |+| _)

    val destLookup =
      xs.collect { case Renderable.Destination(k, vs) => Map(k -> vs) }
        .foldLeft(Map.empty[String, NonEmptyList[String]])(_ |+| _)

    val arrows =
      xs
        .collect { case Renderable.MultiArrow(src, dest) => src -> dest }
        .flatMap { case (srcAlias, destAlias) =>
          (for
            xs <- srcLookup.getOrElse(srcAlias, NonEmptyList.one(srcAlias))
            ys <- destLookup.getOrElse(destAlias, NonEmptyList.one(destAlias))
          yield ev.renderArrow(xs, ys)).toList
        }
        .flatten

    val tagged =
      xs.collect { case x: Renderable.Tagged[A] => x }

    val sourcesDebug =
      srcLookup.toList.map { case (k, vs) => s"Source `$k` rewritten as " + vs.toList.mkString(", ") }

    val destsDebug =
      destLookup.toList.map { case (k, vs) => s"Destination `$k` rewritten as " + vs.toList.mkString(", ") }

    val outs =
      f(tagged ++ arrows) ++ ev.debug(sourcesDebug ++ destsDebug)

    outs
