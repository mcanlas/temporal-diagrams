package com.htmlism.temporaldiagrams

/**
  * Describes an encoding bridge from DSL `A` to diagram dialect `B`
  */
trait DslEncoder[A, B] {
  def encode(x: Renderable[A]): String
}
