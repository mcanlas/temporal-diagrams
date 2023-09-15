package com.htmlism.temporaldiagrams.v2

import scala.annotation.unused

import cats.data.NonEmptyList

sealed trait Renderable[D]

object Renderable {
  case class One[D, A](x: A)(implicit @unused enc: HighlightEncoder[D, A]) extends Renderable[D]

  case class Many[D](xs: NonEmptyList[Renderable[D]]) extends Renderable[D]

  def apply[D](x: Renderable[D], xs: Renderable[D]*): Renderable.Many[D] =
    Many(NonEmptyList(x, xs.toList))
}
