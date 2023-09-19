package com.htmlism.temporaldiagrams.v2

import cats.data.NonEmptyList
import weaver.FunSuite

object RenderableSuite extends FunSuite {
  val explicitRs: NonEmptyList[Renderable[NonEmptyList[ToyDiagramLanguage]]] =
    NonEmptyList.of[Renderable[NonEmptyList[ToyDiagramLanguage]]](
      Renderable.Of[NonEmptyList[ToyDiagramLanguage], Amazon.Ec2](Amazon.Ec2(""), Nil),
      Renderable.Of[NonEmptyList[ToyDiagramLanguage], Google.Compute](Google.Compute(""), Nil)
    )

  test(
    "Domain objects from unrelated hierarchies can be bound together, and can render to their shared target language"
  ) {
    expect.eql(
      NonEmptyList.of[ToyDiagramLanguage](
        ToyDiagramLanguage.Component("amazon ec2: "),
        ToyDiagramLanguage.Component("google compute: ")
      ),
      Renderable.renderMany(explicitRs)
    )
  }
}
