package com.htmlism.temporaldiagrams

import cats.Semigroup

/**
  * A renderable must hold any domain language `A`
  *
  * For highlighting to be supported, every other renderable must support dimming. So maybe every renderable by
  * definition should support two styles
  */
sealed trait Renderable[+A]

object Renderable {
  implicit def renderableSemigroup[A]: Semigroup[Renderable[A]] =
    Cons(_, _)

  /**
    * A renderable that can be targeted by tags for highlighting
    */
  case class Tagged[A](tags: List[String], x: A) extends Renderable[A]

  /**
    * A renderable that can be highlighted by name
    */
  case class Cons[A](x: Renderable[A], y: Renderable[A]) extends Renderable[A]

  def keys[A](x: Renderable[A]): List[String] =
    x match {
      case Tagged(tags, _) =>
        tags

      case Cons(x, y) =>
        keys(x) ::: keys(y)
    }
}
