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

  test("value decoder is a functor and exists for string") {
    val dec =
      ValueDecoder[String]
        .emap(s => s.length.asRight)

    exists(dec.decode("helloworld")) {
      expect.eql(10, _)
    }
  }

  test("value decoder is a functor and exists for int") {
    ValueDecoder[Int]
      .decode("helloworld")
      .fold(succeed, _ => failure("impossible parse"))
  }

  test("syntax exists to link a key and a value decoder") {
    val params =
      Map(
        "key" -> List("foo")
      )

    implicit val dec =
      "key".asValue[String]

    exists(QueryStringDecoder[String].decode(params)) {
      expect.eql("foo", _)
    }
  }

  test("syntax exists to link a name and a record decoder") {
    val params =
      Map(
        "foo.bar" -> List("payload")
      )

    implicit val dec: KeyValuePairsDecoder[Wrapped] =
      "bar".asValue[Wrapped]

    val newDec =
      "foo.".asRecord[Wrapped]

    exists(QueryStringDecoder[Wrapped](newDec).decode(params).map(_.s)) {
      expect.eql("payload", _)
    }
  }
}

case class Wrapped(s: String)

object Wrapped {
  implicit val dec: ValueDecoder[Wrapped] =
    ValueDecoder[String]
      .map(Wrapped(_))
}

case class EmptyRecord()

object EmptyRecord {
  implicit val dec: RecordDecoder[EmptyRecord] =
    (_: Map[String, List[String]]) => EmptyRecord().validNec
}
