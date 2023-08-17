package com.htmlism.temporaldiagrams.v2

import cats.Contravariant
import cats.data.NonEmptyList

/**
  * A type class to encode structures into a diagram language
  *
  * @tparam D
  *   The target diagram language to encode to
  * @tparam A
  *   The data structure being encoded
  */

trait Encoder[D, A] {

  /**
    * The default encoding for a given data structure
    *
    * In practice, it is usually equivalent to the encoding for `encodeWithHighlights` when `highlighted` is `true`
    *
    * @param x
    *   The data structure being encoded
    */
  def encode(x: A): NonEmptyList[String]

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
  def encodeWithHighlights(x: A, highlighted: Boolean): NonEmptyList[String]
}

object Encoder {

  /**
    * @tparam D
    *   The target diagram language to encode to
    */
  implicit def encoderContravariant[D]: Contravariant[Encoder[D, *]] =
    new Contravariant[Encoder[D, *]] {
      def contramap[A, B](fa: Encoder[D, A])(f: B => A): Encoder[D, B] =
        new Encoder[D, B] {
          def encode(x: B): NonEmptyList[String] =
            fa.encode(f(x))

          def encodeWithHighlights(x: B, highlighted: Boolean): NonEmptyList[String] =
            fa.encodeWithHighlights(f(x), highlighted)
        }
    }
}
