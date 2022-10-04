package com.htmlism.temporaldiagrams

/**
  * Describes an encoding bridge from DSL `A` to diagram dialect `B`
  */
trait DslEncoder[A, B] {
  def encode(x: A): List[B]

  def encodeWithHighlights(x: A, highlighted: Boolean): List[B]
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
          ev.encodeWithHighlights(x, true)
        else
          ev.encodeWithHighlights(x, false)
      }
}
