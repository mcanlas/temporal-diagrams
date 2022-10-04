package com.htmlism.temporaldiagrams

import cats.data.NonEmptyList

/**
  * A renderable must hold any domain language `A`
  *
  * For highlighting to be supported, every other renderable must support dimming. So maybe every renderable by
  * definition should support two styles
  */
sealed trait Renderable[+A]

object Renderable {

  /**
    * A renderable that can be targeted by tags for highlighting
    */
  case class Tagged[A](tags: List[String], x: A) extends Renderable[A]

  /**
    * Defines a source for a multi-arrow; from many to "one"
    */
  case class Source(alias: String, sources: NonEmptyList[String]) extends Renderable[Nothing]

  /**
    * Defines a destination for a multi-arrow; from "one" to many
    */
  case class Destination(alias: String, destinations: NonEmptyList[String]) extends Renderable[Nothing]

  /**
    * A series of arrows that is bound/rendered later than the entities; multiply dispatched based on sources and
    * destinations
    */
  case class MultiArrow(src: String, dest: String) extends Renderable[Nothing]

  def keys[A](x: Renderable[A]): List[String] =
    x match {
      case Tagged(tags, _) =>
        tags

      case _ =>
        Nil
    }
}
