package com.htmlism.temporaldiagrams.server

import cats.*
import cats.syntax.all.*

// a named kleisli for `A => Either[String, B]`
// the error channel is linear (as is the case with flatmap refinements
trait ValueDecoder[A]:
  self: ValueDecoder[A] =>
  def decode(s: String): Either[String, A]

  // flatmap for the kleisli
  def emap[B](f: A => Either[String, B]): ValueDecoder[B] =
    (s: String) =>
      self
        .decode(s)
        .flatMap(f)

object ValueDecoder:
  implicit val stringDecoder: ValueDecoder[String] =
    (s: String) => s.asRight

  implicit val intDecoder: ValueDecoder[Int] =
    (s: String) => util.Try(s.toInt).toEither.leftMap(_.toString)

  implicit val functor: Functor[ValueDecoder] =
    new Functor[ValueDecoder]:
      def map[A, B](fa: ValueDecoder[A])(f: A => B): ValueDecoder[B] =
        (s: String) => fa.decode(s).map(f)

  def apply[A](implicit ev: ValueDecoder[A]): ValueDecoder[A] =
    ev
