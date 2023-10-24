package com.htmlism.temporaldiagrams.v2

import cats.Contravariant

/**
  * A type class that describes an encoding relationship for an input domain language into a target diagram language
  *
  * @tparam D
  *   The target diagram language being encoded to
  * @tparam A
  *   The input data type being encoded
  */
trait HighlightEncoder[D, -A]:

  /**
    * The default encoding for a given data structure
    *
    * In practice, it is usually equivalent to the encoding for `encodeWithHighlights` when `highlighted` is `true`
    *
    * @param x
    *   The data structure being encoded
    */
  def encode(x: A): D

  /**
    * The encoding for a given data structure when one portion of the diagram is highlighted and the rest is not
    *
    * In practice, when `highlighted` is `true`, it is equivalent to the encoding from `encode`. When `highlighted` is
    * `false`, the rendering is generally dim or gray, allowing the viewer to focus on the highlighted aspect
    *
    * @param x
    *   The data structure being encoded
    * @param highlighted
    *   True when this data structure is being highlighted by the renderer; false otherwise
    */
  def encodeWithHighlights(x: A, highlighted: Boolean): D

// can this encoder be a kleisli?
object HighlightEncoder:

  /**
    * @tparam D
    *   The target diagram language to encode to
    */
  given [D]: Contravariant[HighlightEncoder[D, *]] with
    def contramap[A, B](fa: HighlightEncoder[D, A])(f: B => A): HighlightEncoder[D, B] =
      new HighlightEncoder[D, B]:
        def encode(x: B): D =
          fa.encode(f(x))

        def encodeWithHighlights(x: B, highlighted: Boolean): D =
          fa.encodeWithHighlights(f(x), highlighted)
