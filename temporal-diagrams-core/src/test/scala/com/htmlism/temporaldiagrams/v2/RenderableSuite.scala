package com.htmlism.temporaldiagrams.v2

import scala.collection.immutable.ListSet

import cats.data.Chain
import weaver.FunSuite

object RenderableSuite extends FunSuite:
  val explicitRs: Chain[Renderable[Chain[ToyDiagramLanguage]]] =
    Chain[Renderable[Chain[ToyDiagramLanguage]]](
      Renderable.OfA[Chain[ToyDiagramLanguage], Amazon.Ec2](Amazon.Ec2(""), ListSet.empty),
      Renderable.OfA[Chain[ToyDiagramLanguage], Google.Compute](Google.Compute(""), ListSet.empty)
    )

  val renderablesWithTags: Chain[Renderable[Chain[ToyDiagramLanguage]]] =
    Chain[Renderable[Chain[ToyDiagramLanguage]]](
      Renderable.OfA[Chain[ToyDiagramLanguage], Amazon.Ec2](Amazon.Ec2("foo"), ListSet("amazon")),
      Renderable.OfA[Chain[ToyDiagramLanguage], Google.Compute](Google.Compute("bar"), ListSet("google"))
    )

  test(
    "Domain objects from unrelated hierarchies can be bound together, and can render to their shared target language"
  ):
    expect.eql(
      Chain[ToyDiagramLanguage](
        ToyDiagramLanguage.Component("amazon ec2: "),
        ToyDiagramLanguage.Component("google compute: ")
      ),
      Renderable.renderMany(explicitRs)
    )

  test("Renderables have tags"):
    expect.same(
      ListSet("amazon", "google"),
      Renderable.allTags(renderablesWithTags)
    )

  test("Can render with highlights"):
    expect.same(
      Chain[ToyDiagramLanguage](
        ToyDiagramLanguage.Component("amazon ec2: foo true"),
        ToyDiagramLanguage.Component("google compute: bar false")
      ),
      Renderable
        .renderManyWithTag(renderablesWithTags, "amazon")
    )
