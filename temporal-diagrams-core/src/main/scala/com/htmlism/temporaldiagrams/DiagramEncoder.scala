package com.htmlism.temporaldiagrams

import cats.Contravariant
import cats.data.Chain

/**
  * A type class to encode structures into a diagram language as strings
  *
  * @tparam A
  *   The diagram language to encode from
  */

trait DiagramEncoder[A]:

  /**
    * The default encoding for a given data structure
    *
    * @param x
    *   The data structure being encoded
    */
  def encode(x: A): Chain[String]

object DiagramEncoder:

  /**
    * @tparam D
    *   The target diagram language to encode to
    */
  given [D]: Contravariant[DiagramEncoder] with
    def contramap[A, B](fa: DiagramEncoder[A])(f: B => A): DiagramEncoder[B] =
      (x: B) => fa.encode(f(x))

  def apply[A: DiagramEncoder]: DiagramEncoder[A] =
    summon[DiagramEncoder[A]]
