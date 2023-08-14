package com.htmlism.temporaldiagrams.v2

/**
  * A type class to encode structures into a diagram language
  *
  * @tparam A
  *   The target diagram language to encode to
  * @tparam B
  *   The specific input type being encoded
  */

trait Encoder[A, B] {
  def encode(x: B): A

  def encodeWithHighlights(x: B, highlighted: Boolean): A
}
