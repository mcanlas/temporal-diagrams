package com.htmlism.temporaldiagrams

/**
  * Describes an encoding bridge from DSL `A` to diagram dialect `B`
  */
trait DslEncoder[A, B] {
  def encodeMonoid(x: Renderable[A]): List[B]

  def encode(x: Renderable[A]): String

  def encodeWithHighlights(x: Renderable[A], highlights: Set[String]): String
}
