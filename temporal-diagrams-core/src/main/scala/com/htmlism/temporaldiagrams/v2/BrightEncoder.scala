package com.htmlism.temporaldiagrams.v2

import cats.data.NonEmptyList

/**
  * A convenience trait that uses one "bright" encoding for both the normal rendering and highlighted rendering
  *
  * @tparam D
  *   The target diagram language to encode to
  * @tparam A
  *   The data structure being encoded
  */
trait BrightEncoder[D, A] extends Encoder[D, A] {

  /**
    * The encoding for both normal rendering and highlighted rendering
    *
    * @param x
    *   The data structure being encoded
    */
  def bright(x: A): NonEmptyList[String]

  /**
    * The encoding for structures that are not highlighted during highlighted rendering
    *
    * @param x
    *   The data structure being encoded
    */
  def dim(x: A): NonEmptyList[String]

  def encode(x: A): NonEmptyList[String] =
    bright(x)

  def encodeWithHighlights(x: A, highlighted: Boolean): NonEmptyList[String] =
    if (highlighted)
      bright(x)
    else
      dim(x)
}
