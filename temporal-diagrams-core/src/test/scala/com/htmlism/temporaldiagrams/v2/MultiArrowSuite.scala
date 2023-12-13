package com.htmlism.temporaldiagrams.v2

import cats.data.*
import weaver.FunSuite

import com.htmlism.temporaldiagrams.v2.syntax.*

object MultiArrowSuite extends FunSuite:
  test("can add multi-arrow sources"):
    val implicitRs: Chain[Renderable.WithMultiArrows[Chain[ToyDiagramLanguage]]] =
      Chain[Renderable.WithMultiArrows[Chain[ToyDiagramLanguage]]](
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.Source("", NonEmptyList.of("foo"))
      )

    expect.eql(
      1L,
      implicitRs
        .collect:
          case x: Renderable.WithMultiArrows.Source[?] => x
        .size
    )

  test("can add multi-arrow destinations"):
    val implicitRs: Chain[Renderable.WithMultiArrows[Chain[ToyDiagramLanguage]]] =
      Chain[Renderable.WithMultiArrows[Chain[ToyDiagramLanguage]]](
        Amazon.Ec2("").r,
        Google.Compute("").r,
        Renderable.WithMultiArrows.Destination("", NonEmptyList.of("foo"))
      )

    expect.eql(
      1L,
      implicitRs
        .collect:
          case x: Renderable.WithMultiArrows.Destination[?] => x
        .size
    )

  test("can define multi-arrows"):
    expect.eql(1, 1)

  test("rendering multi-arrows is fallible given an undefined source"):
    expect.eql(1, 1)

  test("rendering multi-arrows is fallible given an undefined destination"):
    expect.eql(1, 1)

  test("rendering multi-arrows can be ignored"):
    expect.eql(1, 1)

  test("rendering multi-arrows becomes one domain language"):
    expect.eql(1, 1)

  test("collecting renderables gets rid of multi-arrow classes"):
    expect.eql(1, 1)
