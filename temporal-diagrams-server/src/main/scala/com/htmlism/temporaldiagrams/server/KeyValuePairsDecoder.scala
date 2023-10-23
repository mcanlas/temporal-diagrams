package com.htmlism.temporaldiagrams.server

import cats.*
import cats.data.*
import cats.syntax.all.*

sealed trait KeyValuePairsDecoder[A]:
  self: KeyValuePairsDecoder[A] =>
  def decode(xs: Map[String, List[String]], ns: Chain[String]): ValidatedNec[String, A]

  def withNamespace(n: String): KeyValuePairsDecoder[A] =
    new KeyValuePairsDecoder[A]:
      def decode(xs: Map[String, List[String]], ns: Chain[String]): ValidatedNec[String, A] =
        self
          .decode(xs, n +: ns)

object KeyValuePairsDecoder:
  implicit val functor: Functor[KeyValuePairsDecoder] =
    new Functor[KeyValuePairsDecoder]:
      def map[A, B](fa: KeyValuePairsDecoder[A])(f: A => B): KeyValuePairsDecoder[B] =
        new KeyValuePairsDecoder[B]:
          def decode(xs: Map[String, List[String]], ns: Chain[String]): ValidatedNec[String, B] =
            fa.decode(xs, ns).map(f)

  implicit val semigroupal: Semigroupal[KeyValuePairsDecoder] =
    new Semigroupal[KeyValuePairsDecoder]:
      def product[A, B](fa: KeyValuePairsDecoder[A], fb: KeyValuePairsDecoder[B]): KeyValuePairsDecoder[(A, B)] =
        new KeyValuePairsDecoder[(A, B)]:
          def decode(xs: Map[String, List[String]], ns: Chain[String]): ValidatedNec[String, (A, B)] =
            fa.decode(xs, ns) match
              case Validated.Valid(x) =>
                fb.decode(xs, ns) match
                  case Validated.Valid(y) =>
                    (x, y).valid
                  case Validated.Invalid(err) =>
                    err.invalid

              case Validated.Invalid(erry) =>
                fb.decode(xs, ns) match
                  case Validated.Valid(_) =>
                    erry.invalid
                  case Validated.Invalid(errx) =>
                    (errx |+| erry).invalid

  case class One[A](key: String)(using A: ValueDecoder[A]) extends KeyValuePairsDecoder[A]:
    def decode(xs: Map[String, List[String]], ns: Chain[String]): ValidatedNec[String, A] =
      val fullKey =
        (ns :+ key)
          .mkString_(".")

      Either
        .fromOption(xs.get(fullKey), s"key $fullKey did not exist")
        .map(_.headOption)
        .flatMap(Either.fromOption(_, s"available key $fullKey had no values"))
        .flatMap(A.decode)
        .toValidatedNec
