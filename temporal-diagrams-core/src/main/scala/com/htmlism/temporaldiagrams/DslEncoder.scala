package com.htmlism.temporaldiagrams

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

  private def encodeCommon[A, B](xs: List[Renderable[A]])(f: List[Renderable.Tagged[A]] => List[B]): List[B] =
    val tagged =
      xs.collect { case x: Renderable.Tagged[A] => x }

    val outs =
      f(tagged)

    outs
