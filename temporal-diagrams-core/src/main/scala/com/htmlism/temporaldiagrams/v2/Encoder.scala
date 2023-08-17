package com.htmlism.temporaldiagrams.v2

import cats.data.NonEmptyList

/**
  * A type class to encode structures into a diagram language
  *
  * @tparam A
  *   The target diagram language to encode to
  * @tparam B
  *   The specific input type being encoded
  */

trait Encoder[A, B] {
  def encode(x: B): NonEmptyList[String]

  def encodeWithHighlights(x: B, highlighted: Boolean): NonEmptyList[String]
}
