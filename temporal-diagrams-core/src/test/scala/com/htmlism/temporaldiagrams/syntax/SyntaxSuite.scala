package com.htmlism.temporaldiagrams
package syntax

import scala.collection.immutable.ListSet

import cats.data.Chain
import weaver.*

object SyntaxSuite extends FunSuite:
  test("Domain objects from unrelated hierarchies can be bound together, with postfix syntax"):
    val implicitRs =
      Chain[Renderable.Of[Chain[ToyDiagramLanguage]]](
        Amazon.Ec2("").r,
        Google.Compute("").r
      )

    expect.same(RenderableSuite.explicitRs, implicitRs)

  test("Domain objects from unrelated hierarchies can be bound together, with postfix tagging"):
    val tagged =
      Chain[Renderable.Of[Chain[ToyDiagramLanguage]]](
        Amazon.Ec2("").tag("hello"),
        Google.Compute("").r
      )

    expect.same(ListSet("hello"), tagged.toList(0).tags) &&
    expect.same(ListSet.empty, tagged.toList(1).tags)
