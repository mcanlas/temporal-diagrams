package com.htmlism.temporaldiagrams.server

import cats.data.*
import cats.syntax.all.*
import weaver.*

import com.htmlism.temporaldiagrams.server.builder.*

object QueryParamsDecoderSuite extends FunSuite:
  test("a value decoder already exists for string"):
    val dec =
      summon[ValueDecoder[String]]

    exists(dec.decode("foo")):
      expect.eql("foo", _)

  test("a value decoder is a functor"):
    val dec =
      summon[ValueDecoder[String]]
        .map(_.length)

    exists(dec.decode("foo")):
      expect.eql(3, _)

  test("a value decoder can compose fallibility via emap"):
    val dec =
      summon[ValueDecoder[String]]
        .emap(_.asRight)

    exists(dec.decode("foo")):
      expect.eql("foo", _)

  test("value decoder is a functor and exists for string"):
    val dec =
      ValueDecoder[String]
        .emap(s => s.length.asRight)

    exists(dec.decode("helloworld")):
      expect.eql(10, _)

  test("value decoder is a functor and exists for int"):
    ValueDecoder[Int]
      .decode("helloworld")
      .fold(succeed, _ => failure("impossible parse"))

  test("syntax exists to parse a key with a value decoder"):
    val params =
      Map(
        "foo" -> List("bar")
      )

    given KeyValuePairsDecoder[String] =
      "foo".as[String]

    all(
      exists(QueryStringDecoder[String].decode(params)) {
        expect.eql("bar", _)
      },
      matches(QueryStringDecoder[String].decode(Map("" -> Nil))) { case Validated.Invalid(xs) =>
        exists(xs)(x => verify(x.contains("foo did not exist"), "requires key"))
      },
      matches(QueryStringDecoder[String].decode(Map("foo" -> Nil))) { case Validated.Invalid(xs) =>
        exists(xs)(x => verify(x.contains("foo had no values"), "values must be non-empty"))
      }
    )

  test("the pairs decode can compose via mapN"):
    val params =
      Map(
        "name" -> List("alpha"),
        "age"  -> List("123")
      )

    exists(QueryStringDecoder[Person].decode(params)):
      expect.same(Person("alpha", 123), _)

  test("syntax exists to link a namespace and a record"):
    val params =
      Map(
        "one.name" -> List("alpha"),
        "one.age"  -> List("123"),
        "two.name" -> List("beta"),
        "two.age"  -> List("456")
      )

    given KeyValuePairsDecoder[(Person, Person)] =
      (
        "one".namespaces[Person],
        "two".namespaces[Person]
      ).mapN((a, b) => a -> b)

    exists(QueryStringDecoder[(Person, Person)].decode(params)):
      expect.same(Person("alpha", 123) -> Person("beta", 456), _)

  test("record errors accumulate"):
    val params =
      Map(
        "foo" -> List("foo"),
        "bar" -> List("bar")
      )

    given KeyValuePairsDecoder[(Int, Int)] =
      (
        "foo".as[Int],
        "bar".as[Int]
      ).mapN((a, b) => a -> b)

    matches(QueryStringDecoder[(Int, Int)].decode(params)) { case Validated.Invalid(xs) =>
      verify(xs.length == 2)
    }

  test("int parse failure shows exception type"):
    val res =
      "foo".as[Int].decode(Map("foo" -> List("abc")), Chain.empty)

    matches(res) { case Validated.Invalid(xs) =>
      forEach(xs)(x => verify(x.contains("java.lang.NumberFormatException")))
    }

  private def all(xs: Expectations*) =
    xs.reduce(_ && _)

case class Person(name: String, age: Int)

object Person:
  given KeyValuePairsDecoder[Person] =
    ("name".as[String], "age".as[Int])
      .mapN(Person.apply)
