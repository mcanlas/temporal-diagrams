package com.htmlism.temporaldiagrams

/**
  * A renderable must hold any domain language `A`
  *
  * For highlighting to be supported, every other renderable must support dimming. So maybe every renderable by
  * definition should support two styles
  */
sealed trait Renderable[+A]

object Renderable:

  /**
    * A renderable that can be targeted by tags for highlighting
    */
  case class Tagged[+A](tags: List[String], x: A) extends Renderable[A]

  def keys[A](x: Renderable[A]): List[String] =
    x match
      case Tagged(tags, _) =>
        tags
