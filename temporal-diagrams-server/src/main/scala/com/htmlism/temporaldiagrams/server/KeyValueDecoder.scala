package com.htmlism.temporaldiagrams.server

import cats._
import cats.data._
import cats.syntax.all._

// TODO should be a codec actually...
sealed trait KeyValueDecoder[A] { self: KeyValueDecoder[A] =>
  def decode(xs: Map[String, List[String]], ns: List[String]): ValidatedNec[String, A]

  // TODO thread in the namespace into the errors, maybe changing the signature of `decode`
  def withNamespace(n: String): KeyValueDecoder[A] =
    new KeyValueDecoder[A] {
      def decode(xs: Map[String, List[String]], ns: List[String]): ValidatedNec[String, A] =
        self
          .decode(xs.view.filterKeys(_.startsWith(n)).toMap, n :: ns)
    }
}

object KeyValueDecoder {
  implicit val semigroupal: Semigroupal[KeyValueDecoder] =
    new Semigroupal[KeyValueDecoder] {
      def product[A, B](fa: KeyValueDecoder[A], fb: KeyValueDecoder[B]): KeyValueDecoder[(A, B)] =
        new KeyValueDecoder[(A, B)] {
          def decode(xs: Map[String, List[String]], ns: List[String]): ValidatedNec[String, (A, B)] =
            fa.decode(xs, ns) match {
              case Validated.Valid(x) =>
                fb.decode(xs, ns) match {
                  case Validated.Valid(y) =>
                    (x, y).valid
                  case Validated.Invalid(err) =>
                    err.invalid
                }

              case Validated.Invalid(erry) =>
                fb.decode(xs, ns) match {
                  case Validated.Valid(_) =>
                    erry.invalid
                  case Validated.Invalid(errx) =>
                    (errx |+| erry).invalid
                }
            }
        }
    }

  case class Value[A](key: String)(implicit A: ValueDecoder[A]) extends KeyValueDecoder[A] {
    def decode(xs: Map[String, List[String]], ns: List[String]): ValidatedNec[String, A] =
      Either
        .fromOption(xs.get(ns.mkString("") + key), s"key $key did not exist")
        .map(xs => xs.headOption.getOrElse(s"available key $key had no values"))
        .flatMap(A.decode)
        .toValidatedNec
  }

  case class Record[A](namespace: String)(implicit A: RecordDecoder[A]) extends KeyValueDecoder[A] {
    def decode(xs: Map[String, List[String]], ns: List[String]): ValidatedNec[String, A] =
      A.decode(xs)
  }
}
