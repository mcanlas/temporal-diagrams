package com.htmlism.temporaldiagrams

import cats.data.NonEmptyList

/**
  * Describes an encoding bridge from DSL `A` to diagram dialect `B`
  */
trait DslEncoder[A, B] {
  def encode(x: A): List[B]

  def encodeWithHighlights(x: A, highlighted: Boolean): List[B]

  def encodeArrow(src: String, dest: String): List[B]
}

object DslEncoder {
  def encodeMany[A, B](xs: List[Renderable.Tagged[A]])(implicit ev: DslEncoder[A, B]): List[B] =
    xs
      .map(_.x)
      .flatMap(ev.encode)

  def encodeManyWithHighlights[A, B](xs: List[Renderable.Tagged[A]], highlights: String*)(implicit
      ev: DslEncoder[A, B]
  ): List[B] =
    xs
      .flatMap { case Renderable.Tagged(tags, x) =>
        if ((highlights intersect tags).nonEmpty)
          ev.encodeWithHighlights(x, highlighted = true)
        else
          ev.encodeWithHighlights(x, highlighted = false)
      }

  object Multi {
    def encode[A, B](xs: List[Renderable[A]])(implicit ev: DslEncoder[A, B]): List[B] = {
      val srcLookup =
        xs.collect { case Renderable.Source(k, vs) => k -> vs }.toMap

      val destLookup =
        xs.collect { case Renderable.Destination(k, vs) => k -> vs }.toMap

      val arrows =
        xs
          .collect { case Renderable.MultiArrow(src, dest) => src -> dest }
          .flatMap { case (srcAlias, destAlias) =>
            (for {
              xs <- srcLookup.getOrElse(srcAlias, NonEmptyList.one(srcAlias))
              ys <- destLookup.getOrElse(destAlias, NonEmptyList.one(destAlias))
            } yield ev.encodeArrow(xs, ys)).toList
          }
          .flatten

      val tagged =
        xs.collect { case x: Renderable.Tagged[A] => x }

      val outs =
        encodeMany(tagged) ++ arrows

      outs
    }
  }
}
