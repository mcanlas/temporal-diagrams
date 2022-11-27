package com.htmlism.temporaldiagrams.server

import cats._
import cats.data._
import cats.syntax.all._

sealed trait KeyValueDecoder[A] { self: KeyValueDecoder[A] =>
  def decode(xs: Map[String, List[String]]): ValidatedNec[String, A]

  // TODO if the collection xs is filtered, the lead string must also be trimmed
  // or thread the namespace into the call
  def withNamespace(n: String): KeyValueDecoder[A] =
    new KeyValueDecoder[A] {
      def decode(xs: Map[String, List[String]]): ValidatedNec[String, A] =
        self
          .decode(xs.view.filterKeys(_.startsWith(n)).toMap)
          .leftMap(_.map(s"within $n" + _))
    }
}

object KeyValueDecoder {
  implicit val semigroupal: Semigroupal[KeyValueDecoder] =
    new Semigroupal[KeyValueDecoder] {
      def product[A, B](fa: KeyValueDecoder[A], fb: KeyValueDecoder[B]): KeyValueDecoder[(A, B)] =
        new KeyValueDecoder[(A, B)] {
          def decode(xs: Map[String, List[String]]): ValidatedNec[String, (A, B)] =
            fa.decode(xs) match {
              case Validated.Valid(x) =>
                fb.decode(xs) match {
                  case Validated.Valid(y) =>
                    (x, y).valid
                  case Validated.Invalid(err) =>
                    err.invalid
                }

              case Validated.Invalid(erry) =>
                fb.decode(xs) match {
                  case Validated.Valid(_) =>
                    erry.invalid
                  case Validated.Invalid(errx) =>
                    (errx |+| erry).invalid
                }
            }
        }
    }

  case class One[A](key: String)(implicit A: ValueDecoder[A]) extends KeyValueDecoder[A] {
    def decode(xs: Map[String, List[String]]): ValidatedNec[String, A] =
      Either
        .fromOption(xs.get(key), s"key $key did not exist")
        .map(xs => xs.headOption.getOrElse(s"available key $key had no values"))
        .flatMap(A.decode)
        .toValidatedNec
  }
}
