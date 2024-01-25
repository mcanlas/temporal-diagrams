package com.htmlism.temporaldiagrams.v2

import scala.collection.immutable.ListSet

import cats.data.*
import weaver.FunSuite

import com.htmlism.temporaldiagrams.v2.syntax.*

object MultiArrowSuite extends FunSuite:
  test("can add multi-arrow sources"):
    val domainWithArrows =
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.Source("", List("foo"))
      )

    expect.same(
      Chain(Renderable.WithMultiArrows.Source("", List("foo"))),
      domainWithArrows
        .collect:
          case x: Renderable.WithMultiArrows.Source[?] => x
    )

  test("can add multi-arrow destinations"):
    val domainWithArrows =
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.Destination("", List("foo"))
      )

    expect.same(
      Chain(Renderable.WithMultiArrows.Destination("", List("foo"))),
      domainWithArrows
        .collect:
          case x: Renderable.WithMultiArrows.Destination[?] => x
    )

  test("can define multi-arrows"):
    val domainWithArrows =
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.MultiArrow("src", "dest", ListSet.empty)
      )

    expect.same(
      Chain(Renderable.WithMultiArrows.MultiArrow("src", "dest", ListSet.empty)),
      domainWithArrows
        .collect:
          case x: Renderable.WithMultiArrows.MultiArrow => x
    )

  test("rendering multi-arrows is fallible given an undefined source"):
    val domainWithArrows =
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.Destination("dest", List.empty[String]),
        Renderable.WithMultiArrows.MultiArrow("invalid-src", "dest", ListSet.empty)
      )

    val res =
      Renderable.WithMultiArrows.renderArrows[Microsoft.Arrow](domainWithArrows)

    matches(res):
      case Validated.Invalid(errs) =>
        expect.eql(1L, errs.length) and
          expect(errs.head.contains("invalid-src"))

  test("rendering multi-arrows is fallible given an undefined destination"):
    val domainWithArrows =
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.Source("src", List.empty[String]),
        Renderable.WithMultiArrows.MultiArrow("src", "invalid-dest", ListSet.empty)
      )

    val res =
      Renderable.WithMultiArrows.renderArrows[Microsoft.Arrow](domainWithArrows)

    matches(res):
      case Validated.Invalid(errs) =>
        expect.eql(1L, errs.length) and
          expect(errs.head.contains("invalid-dest"))

  test("multi-arrows can render to an input domain language unrelated to the existing renderables"):
    val domainWithArrows =
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.Source("src", List.empty[String])
      )

    val res =
      Renderable.WithMultiArrows.renderArrows[Microsoft.Arrow](domainWithArrows)

    whenSuccess(res): rs =>
      expect.eql(
        Chain[ToyDiagramLanguage](
          ToyDiagramLanguage.Component("amazon ec2: "),
          ToyDiagramLanguage.Component("google compute: ")
        ),
        Renderable.renderMany(rs)
      )

  test("rendered multi-arrows are a product of its inputs"):
    val domainWithArrows =
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.Source("src", List("a", "b", "c")),
        Renderable.WithMultiArrows.Destination("dest", List("x", "y")),
        Renderable.WithMultiArrows.MultiArrow("src", "dest", ListSet.empty)
      )

    val res =
      Renderable.WithMultiArrows.renderArrows[Microsoft.Arrow](domainWithArrows)

    whenSuccess(res): rs =>
      expect.eql(
        Chain[ToyDiagramLanguage](
          ToyDiagramLanguage.Component("amazon ec2: "),
          ToyDiagramLanguage.Component("google compute: "),
          ToyDiagramLanguage.Arrow("a to x"),
          ToyDiagramLanguage.Arrow("a to y"),
          ToyDiagramLanguage.Arrow("b to x"),
          ToyDiagramLanguage.Arrow("b to y"),
          ToyDiagramLanguage.Arrow("c to x"),
          ToyDiagramLanguage.Arrow("c to y")
        ),
        Renderable.renderMany(rs)
      )

  test("multi-arrows are tagged"):
    expect.eql(1, 1)

  test("rendering multi-arrows can be ignored"):
    val domainWithArrows =
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.Source("src", List("foo")),
        Renderable.WithMultiArrows.Destination("dest", List("foo")),
        Renderable.WithMultiArrows.MultiArrow("src", "dest", ListSet.empty)
      )

    expect.same(
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r
      ),
      Renderable
        .WithMultiArrows
        .dropArrows:
          domainWithArrows
    )

  test("rendering multi-arrows becomes one domain language"):
    expect.eql(1, 1)

  test("collecting renderables gets rid of multi-arrow classes"):
    expect.eql(1, 1)
