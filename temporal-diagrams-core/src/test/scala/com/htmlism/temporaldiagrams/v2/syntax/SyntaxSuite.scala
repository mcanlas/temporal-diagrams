package com.htmlism.temporaldiagrams.v2
package syntax

import cats.data.NonEmptyList
import weaver._

object SyntaxSuite extends FunSuite {
  test("Domain objects from unrelated hierarchies can be bound together, implicitly") {
    val implicitRs =
      Renderable[NonEmptyList[ToyDiagramLanguage]](
        Amazon.Ec2(""),
        Google.Compute("")
      )

    expect.same(RenderableSuite.explicitRs, implicitRs)
  }
}
