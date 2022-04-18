package com.htmlism.temporaldiagrams

import cats._

/**
 * Just saying `Temporal` clashes with `cats-effect` =(
 */
sealed trait TemporalFrame[K, A]

object TemporalFrame {
  implicit def temporalSemigroup[K, A]: Semigroup[TemporalFrame[K, A]] =
    Cons(_, _)

  def apply[K, A](xs: (K, Renderable[A])*): TemporalFrame[K, A] =
    Indexed(xs)

  case class Indexed[K, A](xs: Seq[(K, Renderable[A])]) extends TemporalFrame[K, A]

  case class Fixed[K, A](x: Renderable[A]) extends TemporalFrame[K, A]

  case class Cons[K, A](x: TemporalFrame[K, A], y: TemporalFrame[K, A]) extends TemporalFrame[K, A]

  def keys[K, A](t: TemporalFrame[K, A])(implicit ord: Ordering[K]): List[K] =
    t match {
      case Cons(x, y) =>
        (keys(x) ::: keys(y)).distinct

      case Fixed(_) =>
        Nil

      case Indexed(xs) =>
        xs.toList
          .map(_._1)
          .distinct
    }

  def resolve[K, A](t: TemporalFrame[K, A], k: K)(implicit ord: Ordering[K]): Renderable[A] =
    t match {
      case Cons(x, y) =>
        Renderable.Cons(resolve(x, k), resolve(y, k))

      case Fixed(x) =>
        x

      case Indexed(xs) =>
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
