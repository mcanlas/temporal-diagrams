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

  test("syntax exists to parse a key with a value decoder") {
    val params =
      Map(
        "key" -> List("foo")
      )

    implicit val dec: KeyValuePairsDecoder[String] =
      "key".as[String]

    exists(QueryStringDecoder[String].decode(params)) {
      expect.eql("foo", _)
    }
  }

  test("the pairs decode can compose via mapN") {
    val params =
      Map(
        "name" -> List("alpha"),
        "age" -> List("123")
      )

    exists(QueryStringDecoder[Person].decode(params)) {
      expect.same(Person("alpha", 123), _)
    }
  }

  test("syntax exists to link a namespace and a record") {
    val params =
      Map(
        "one.name" -> List("alpha"),
        "one.age" -> List("123"),
        "two.name" -> List("beta"),
        "two.age" -> List("456")
      )

    implicit val dec: KeyValuePairsDecoder[(Person, Person)] =
      (
        "one".namespaces[Person],
        "two".namespaces[Person]
      ).mapN((a, b) => a -> b)

    exists(QueryStringDecoder[(Person, Person)].decode(params)) {
      expect.same(Person("alpha", 123) -> Person("beta", 456), _)
    }
  }
}

case class Person(name: String, age: Int)

object Person {
  implicit val dec: KeyValuePairsDecoder[Person] =
    ("name".as[String], "age".as[Int])
      .mapN(Person.apply)
}

case class EmptyRecord()

object EmptyRecord {
  implicit val dec: RecordDecoder[EmptyRecord] =
    (_: Map[String, List[String]]) => EmptyRecord().validNec
}
