package com.htmlism.temporaldiagrams.v2

import cats.data.NonEmptyList
import weaver.FunSuite

object RenderableSuite extends FunSuite {
  val explicitRs: Renderable[NonEmptyList[ToyDiagramLanguage]] =
    Renderable[NonEmptyList[ToyDiagramLanguage]](
      Renderable.One(Amazon.Ec2("")),
      Renderable.One(Google.Compute(""))
    )

  test(
    "Domain objects from unrelated hierarchies can be bound together, and can render to their shared target language"
  ) {
    expect.eql(
      NonEmptyList.of(
        ToyDiagramLanguage.Component("amazon ec2: "),
        ToyDiagramLanguage.Component("google compute: ")
      ),
      Renderable.render(explicitRs)
    )
  }
}
