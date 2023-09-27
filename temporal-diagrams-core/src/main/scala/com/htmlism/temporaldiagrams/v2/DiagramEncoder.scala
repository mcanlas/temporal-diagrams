package com.htmlism.temporaldiagrams.v2

import cats.Contravariant
import cats.data.NonEmptyChain

/**
  * A type class to encode structures into a diagram language as strings
  *
  * @tparam A
  *   The diagram language to encode from
  */

trait DiagramEncoder[A] {

  /**
    * The default encoding for a given data structure
    *
    * @param x
    *   The data structure being encoded
    */
  def encode(x: A): NonEmptyChain[String]
}

object DiagramEncoder {

  /**
    * @tparam D
    *   The target diagram language to encode to
    */
  implicit def encoderContravariant[D]: Contravariant[DiagramEncoder] =
    new Contravariant[DiagramEncoder] {
      def contramap[A, B](fa: DiagramEncoder[A])(f: B => A): DiagramEncoder[B] =
        (x: B) => fa.encode(f(x))
    }

  def apply[A: DiagramEncoder]: DiagramEncoder[A] =
    implicitly[DiagramEncoder[A]]
}
