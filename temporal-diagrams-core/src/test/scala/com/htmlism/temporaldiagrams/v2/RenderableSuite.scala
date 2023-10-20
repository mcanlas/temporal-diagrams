package com.htmlism.temporaldiagrams.v2

import scala.collection.immutable.ListSet

import cats.data.NonEmptyChain
import weaver.FunSuite

object RenderableSuite extends FunSuite {
  val explicitRs: NonEmptyChain[Renderable[NonEmptyChain[ToyDiagramLanguage]]] =
    NonEmptyChain.of[Renderable[NonEmptyChain[ToyDiagramLanguage]]](
      Renderable.OfA[NonEmptyChain[ToyDiagramLanguage], Amazon.Ec2](Amazon.Ec2(""), ListSet.empty),
      Renderable.OfA[NonEmptyChain[ToyDiagramLanguage], Google.Compute](Google.Compute(""), ListSet.empty)
    )

  val renderablesWithTags: NonEmptyChain[Renderable[NonEmptyChain[ToyDiagramLanguage]]] =
    NonEmptyChain.of[Renderable[NonEmptyChain[ToyDiagramLanguage]]](
      Renderable.OfA[NonEmptyChain[ToyDiagramLanguage], Amazon.Ec2](Amazon.Ec2("foo"), ListSet("amazon")),
      Renderable.OfA[NonEmptyChain[ToyDiagramLanguage], Google.Compute](Google.Compute("bar"), ListSet("google"))
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

  test("Renderables have tags") {
    expect.same(
      ListSet("amazon", "google"),
      Renderable.allTags(renderablesWithTags)
    )
  }

  test("Can render with highlights") {
    expect.same(
      NonEmptyChain.of[ToyDiagramLanguage](
        ToyDiagramLanguage.Component("amazon ec2: foo true"),
        ToyDiagramLanguage.Component("google compute: bar false")
      ),
      Renderable
        .renderManyWithTag(renderablesWithTags, "amazon")
    )
  }
}
