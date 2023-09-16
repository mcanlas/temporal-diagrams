package com.htmlism.temporaldiagrams.v2

import cats.Contravariant
import cats.data.NonEmptyList

/**
  * A type class to encode structures into a diagram language as strings
  *
  * @tparam D
  *   The target diagram language to encode to
  * @tparam A
  *   The data structure being encoded
  */

trait DiagramEncoder[D, A] {

  /**
    * The default encoding for a given data structure
    *
    * @param x
    *   The data structure being encoded
    */
  def encode(x: A): NonEmptyList[String]
}

object DiagramEncoder {

  /**
    * @tparam D
    *   The target diagram language to encode to
    */
  implicit def encoderContravariant[D]: Contravariant[DiagramEncoder[D, *]] =
    new Contravariant[DiagramEncoder[D, *]] {
      def contramap[A, B](fa: DiagramEncoder[D, A])(f: B => A): DiagramEncoder[D, B] =
        new DiagramEncoder[D, B] {
          def encode(x: B): NonEmptyList[String] =
            fa.encode(f(x))
        }
    }
}
