package com.htmlism.temporaldiagrams

import cats._

sealed trait Temporal[+K, A]

object Temporal {
  implicit def temporalSemigroup[K, A]: Semigroup[Temporal[K, A]] =
    Cons(_, _)

  def apply[K, A](xs: (K, Renderable[A])*): Temporal[K, A] =
    IndexedTemporal(xs)

  case class IndexedTemporal[K, A](xs: Seq[(K, Renderable[A])]) extends Temporal[K, A]

  case class FixedTemporal[A](x: Renderable[A]) extends Temporal[Nothing, A]

  case class Cons[K, A](x: Temporal[K, A], y: Temporal[K, A]) extends Temporal[K, A]

  def resolve[K, A](t: Temporal[K, A], k: K)(implicit ord: Ordering[K]): Renderable[A] =
    t match {
      case Cons(x, y) =>
        Renderable.Cons(resolve(x, k), resolve(y, k))

      case FixedTemporal(x) =>
        x

      case IndexedTemporal(xs) =>
        val sorted =
          xs
            .sortBy(_._1)

        sorted
          .takeWhile(n => ord.lteq(n._1, k))
          .lastOption
          .map(_._2)
          .getOrElse(sorted.head._2)
    }
}
