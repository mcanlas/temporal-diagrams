package com.htmlism.temporaldiagrams.v2
package syntax

import weaver._

object SyntaxSuite extends FunSuite {
  test("Domain objects from unrelated hierarchies can be bound together, implicitly") {
    val _ =
      Renderable[ToyDiagramLanguage](
        Amazon.Ec2(""),
        Google.Compute("")
      )

    expect.eql(1, 1)
  }
}
