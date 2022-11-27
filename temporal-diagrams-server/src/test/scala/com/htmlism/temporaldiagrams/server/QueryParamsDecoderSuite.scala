package com.htmlism.temporaldiagrams.server

import cats.syntax.all._
import weaver._

import com.htmlism.temporaldiagrams.server.builder._

object QueryParamsDecoderSuite extends FunSuite {
  test("a value decoder already exists for string") {
    val dec =
      implicitly[ValueDecoder[String]]

    exists(dec.decode("foo")) {
      expect.eql("foo", _)
    }
  }

  test("a value decoder is a functor") {
    val dec =
      implicitly[ValueDecoder[String]]
        .map(_.length)

    exists(dec.decode("foo")) {
      expect.eql(3, _)
    }
  }

  test("a value decoder can compose fallibility via emap") {
    val dec =
      implicitly[ValueDecoder[String]]
        .emap(_.asRight)

    exists(dec.decode("foo")) {
      expect.eql("foo", _)
    }
  }

  test("syntax exists to link a key and a value decoder") {
    expect.eql(
      List("foo"),
      "foo".withValue[String].namespaces
    )
  }

  test("syntax exists to link a name and a record decoder") {
    implicit val dec =
      "foo".withValue[Wrapped]

    expect.eql(
      List("bar", "foo"),
      "bar".withRecord[Wrapped].namespaces
    )
  }
}

case class Wrapped(s: String)

object Wrapped {
  implicit val dec: ValueDecoder[Wrapped] =
    implicitly[ValueDecoder[String]].map(Wrapped(_))
}

case class EmptyRecord()

object EmptyRecord {
  implicit val dec: RecordDecoder[EmptyRecord] =
    (_: Map[String, List[String]]) => EmptyRecord().validNec
}
