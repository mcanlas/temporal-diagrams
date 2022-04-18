package com.htmlism.temporaldiagrams

/**
  * Describes an encoding bridge from DSL `A` to diagram dialect `B`
  */
trait DslEncoder[A, B] {
  def injectedStyle: String

  def encode(x: Renderable[A]): List[B]

  def encodeWithHighlights(x: Renderable[A], highlights: Set[String]): List[B]
}
