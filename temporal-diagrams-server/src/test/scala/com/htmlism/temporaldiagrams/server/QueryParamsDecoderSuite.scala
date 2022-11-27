package com.htmlism.temporaldiagrams.server

import cats.syntax.all._
import weaver._

import com.htmlism.temporaldiagrams.server.builder._

object QueryParamsDecoderSuite extends FunSuite with MatchesSyntax {
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
    val params =
      Map(
        "key" -> List("foo")
      )

    val dec = "key".withValue[String]

    exists(dec.decode(params)) {
      expect.eql("foo", _)
    }
  }

  test("syntax exists to link a name and a record decoder") {
    val params =
      Map(
        "foo.bar" -> List("payload")
      )

    implicit val dec: KeyValueDecoder[Wrapped] =
      "bar".withValue[Wrapped]

    val newDec =
      "foo.".withRecord[Wrapped]

    println(dec.decode(params))

    // TODO
    exists(newDec.decode(params).map(_.s)) {
      expect.eql("payload", _)
    }

    expect.eql(1, 1)
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
