package com.htmlism.temporaldiagrams.v2

import cats.Semigroup
import cats.data.NonEmptyList

sealed trait Renderable[D]

object Renderable {
  case class One[D, A](x: A)(implicit enc: HighlightEncoder[D, A]) extends Renderable[D] {
    def render: D =
      enc.encode(x)
  }

  case class Many[D](xs: NonEmptyList[Renderable[D]]) extends Renderable[D]

  def apply[D](x: Renderable[D], xs: Renderable[D]*): Renderable.Many[D] =
    Many(NonEmptyList(x, xs.toList))

  def renderMany[D: Semigroup](x: Renderable[D]): D =
    x match {
      case one @ One(_) =>
        one.render

      case Many(xs) =>
        xs.map(renderMany(_)).reduce
    }
}
