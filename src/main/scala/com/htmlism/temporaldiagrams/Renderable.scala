package com.htmlism.temporaldiagrams

import cats.Semigroup

/**
  * A renderable must hold any domain language `A`
  *
  * For highlighting to be supported, every other renderable must support dimming. So maybe every renderable by
  * definition should support two styles
  */
sealed trait Renderable[A]

/**
  * A renderable that can only be dimmed by others
  */
case class ConstantRenderable[A](x: A) extends Renderable[A]

/**
  * A renderable that can be highlighted by name
  */
case class IdentifiedRenderable[A](id: String, x: A) extends Renderable[A]

/**
  * A renderable that can be highlighted by name
  */
case class RenderableCons[A](x: Renderable[A], y: Renderable[A]) extends Renderable[A]

object Renderable {
  implicit def renderableSemigroup[A]: Semigroup[Renderable[A]] =
    RenderableCons(_, _)
}
