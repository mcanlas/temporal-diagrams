package com.htmlism.temporaldiagrams.v2

import scala.collection.immutable.ListSet

import cats.data.NonEmptyChain
import weaver.FunSuite

object RenderableSuite extends FunSuite {
  val explicitRs: NonEmptyChain[Renderable[NonEmptyChain[ToyDiagramLanguage]]] =
    NonEmptyChain.of[Renderable[NonEmptyChain[ToyDiagramLanguage]]](
      Renderable.Of[NonEmptyChain[ToyDiagramLanguage], Amazon.Ec2](Amazon.Ec2(""), ListSet.empty),
      Renderable.Of[NonEmptyChain[ToyDiagramLanguage], Google.Compute](Google.Compute(""), ListSet.empty)
    )

  test(
    "Domain objects from unrelated hierarchies can be bound together, and can render to their shared target language"
  ) {
    expect.eql(
      NonEmptyChain.of[ToyDiagramLanguage](
        ToyDiagramLanguage.Component("amazon ec2: "),
        ToyDiagramLanguage.Component("google compute: ")
      ),
      Renderable.renderMany(explicitRs)
    )
  }
}
