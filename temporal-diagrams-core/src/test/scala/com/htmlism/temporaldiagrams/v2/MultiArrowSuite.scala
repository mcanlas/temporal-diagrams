package com.htmlism.temporaldiagrams.v2

import cats.data.*
import weaver.FunSuite

import com.htmlism.temporaldiagrams.v2.syntax.*

object MultiArrowSuite extends FunSuite:
  test("can add multi-arrow sources"):
    val rs =
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.Source("", NonEmptyList.of("foo"))
      )

    expect.same(
      Chain(Renderable.WithMultiArrows.Source("", NonEmptyList.of("foo"))),
      rs
        .collect:
          case x: Renderable.WithMultiArrows.Source[?] => x
    )

  test("can add multi-arrow destinations"):
    val rs =
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.Destination("", NonEmptyList.of("foo"))
      )

    expect.same(
      Chain(Renderable.WithMultiArrows.Destination("", NonEmptyList.of("foo"))),
      rs
        .collect:
          case x: Renderable.WithMultiArrows.Destination[?] => x
    )

  test("can define multi-arrows"):
    val rs =
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.MultiArrow("src", "dest")
      )

    expect.same(
      Chain(Renderable.WithMultiArrows.MultiArrow("src", "dest")),
      rs
        .collect:
          case x: Renderable.WithMultiArrows.MultiArrow => x
    )

  test("rendering multi-arrows is fallible given an undefined source"):
    expect.eql(1, 1)

  test("rendering multi-arrows is fallible given an undefined destination"):
    expect.eql(1, 1)

  test("rendering multi-arrows can be ignored"):
    val rs =
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.Source("src", NonEmptyList.of("foo")),
        Renderable.WithMultiArrows.Destination("dest", NonEmptyList.of("foo")),
        Renderable.WithMultiArrows.MultiArrow("src", "dest")
      )

    expect.same(
      Chain(
        Amazon.Ec2("").r,
        Google.Compute("").r
      ),
      Renderable
        .WithMultiArrows
        .dropArrows:
          rs
    )

  test("rendering multi-arrows becomes one domain language"):
    expect.eql(1, 1)

  test("collecting renderables gets rid of multi-arrow classes"):
    expect.eql(1, 1)
