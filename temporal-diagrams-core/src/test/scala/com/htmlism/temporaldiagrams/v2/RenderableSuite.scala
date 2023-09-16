package com.htmlism.temporaldiagrams.v2

import cats.data.NonEmptyList
import weaver.FunSuite

import com.htmlism.temporaldiagrams.v2.ToyDiagramLanguage.Component

object RenderableSuite extends FunSuite {
  test("A renderable can render to its target language") {
    // uses explicit syntax
    val rs =
      Renderable[NonEmptyList[ToyDiagramLanguage]](
        Renderable.One(Amazon.Ec2("")),
        Renderable.One(Google.Compute(""))
      )

    expect.eql(
      NonEmptyList.of(
        Component("amazon ec2: "),
        Component("google compute: ")
      ),
      Renderable.render(rs)
    )
  }
}
