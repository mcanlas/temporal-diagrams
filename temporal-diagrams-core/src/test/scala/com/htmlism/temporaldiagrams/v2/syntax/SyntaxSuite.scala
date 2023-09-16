package com.htmlism.temporaldiagrams.v2
package syntax

import cats.data.NonEmptyList
import weaver._

object SyntaxSuite extends FunSuite {
  test("Domain objects from unrelated hierarchies can be bound together, with postfix syntax") {
    val implicitRs =
      NonEmptyList.of[Renderable[NonEmptyList[ToyDiagramLanguage]]](
        Amazon.Ec2("").r,
        Google.Compute("").r
      )

    expect.same(RenderableSuite.explicitRs, implicitRs)
  }

  test("Domain objects from unrelated hierarchies can be bound together, with postfix tagging") {
    val tagged =
      NonEmptyList.of(
        Amazon.Ec2("").tag[ToyDiagramLanguage]("hello"),
        Google.Compute("").r
      )

    expect.eql(List("hello"), tagged.head.tags) &&
    expect.eql(Nil, tagged.toList(1).tags)
  }
}
