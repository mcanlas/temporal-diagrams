package com.htmlism.temporaldiagrams

import scala.collection.immutable.ListSet

import cats.data.*
import weaver.FunSuite

import com.htmlism.temporaldiagrams.syntax.*

object MultiArrowSuite extends FunSuite:
  given HighlightEncoder[Chain[ToyDiagramLanguage], Unit] with
    def encode(x: Unit): Chain[ToyDiagramLanguage] =
      Chain:
        ToyDiagramLanguage.Arrow("")

    def encodeWithHighlights(x: Unit, highlighted: Boolean): Chain[ToyDiagramLanguage] =
      Chain:
        ToyDiagramLanguage.Arrow("")

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
    // because inline lambdas are anonymous and different
    val constantCallback =
      (_: Nothing, _: Nothing) => ()

    val domainWithArrows =
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.MultiArrow("src", "dest", constantCallback, ListSet.empty)
      )

    expect.same(
      Chain(Renderable.WithMultiArrows.MultiArrow("src", "dest", constantCallback, ListSet.empty)),
      domainWithArrows
        .collect:
          case x: Renderable.WithMultiArrows.MultiArrow[?, ?, ?] => x
    )

  test("rendering multi-arrows is fallible given an undefined source"):
    val domainWithArrows =
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.Destination("dest", List.empty[String]),
        Renderable.WithMultiArrows.MultiArrow("invalid-src", "dest", (_, _) => (), ListSet.empty)
      )

    val res =
      Renderable.WithMultiArrows.renderArrows(domainWithArrows)

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
        Renderable.WithMultiArrows.MultiArrow("src", "invalid-dest", (_, _) => (), ListSet.empty)
      )

    val res =
      Renderable.WithMultiArrows.renderArrows(domainWithArrows)

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
      Renderable.WithMultiArrows.renderArrows(domainWithArrows)

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
        Renderable
          .WithMultiArrows
          .MultiArrow(
            "src",
            "dest",
            (src: String, dest: String) => Microsoft.Arrow(src, dest): Microsoft.Arrow,
            ListSet.empty
          )
      )

    val res =
      Renderable.WithMultiArrows.renderArrows(domainWithArrows)

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
        Renderable.WithMultiArrows.MultiArrow("src", "dest", (_, _) => (), ListSet.empty)
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
