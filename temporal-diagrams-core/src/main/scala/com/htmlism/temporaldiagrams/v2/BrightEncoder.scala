package com.htmlism.temporaldiagrams.v2

/**
  * A convenience trait that uses one "bright" encoding for both the normal rendering and highlighted rendering
  *
  * @tparam D
  *   The target diagram language being encoded to
  * @tparam A
  *   The input data type being encoded
  */
trait BrightEncoder[D, -A] extends HighlightEncoder[D, A]:

  /**
    * Implemented as one method (as opposed to two) to facilitate code sharing between bright and dim, where usually the
    * core semantic data is constant but only the style data is different
    */
  def encodeBrightly(x: A, isBright: Boolean): D

  def encode(x: A): D =
    encodeBrightly(x, isBright = true)

  def encodeWithHighlights(x: A, highlighted: Boolean): D =
    encodeBrightly(x, highlighted)
