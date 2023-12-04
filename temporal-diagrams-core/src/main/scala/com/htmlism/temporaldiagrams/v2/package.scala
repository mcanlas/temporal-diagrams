package com.htmlism.temporaldiagrams

import cats.Monoid
import cats.data.Chain

package object v2:
  def intersperse[A, M: Monoid](xs: Chain[A], f: A => Chain[M]): Chain[M] =
    xs.uncons match
      case Some(head, tail) =>
        f(head)
          .concat(tail.flatMap(x => Monoid[M].empty +: f(x)))

      case None =>
        Chain.empty
