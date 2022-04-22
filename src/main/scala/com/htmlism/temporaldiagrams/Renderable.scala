package com.htmlism.temporaldiagrams

/**
  * A renderable must hold any domain language `A`
  *
  * For highlighting to be supported, every other renderable must support dimming. So maybe every renderable by
  * definition should support two styles
  */
sealed trait Renderable[+A]

object Renderable {
  case object Empty extends Renderable[Nothing]

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
      case Empty =>
        Nil

      case Tagged(tags, _) =>
        tags

      case Cons(x, y) =>
        keys(x) ::: keys(y)
    }
}
