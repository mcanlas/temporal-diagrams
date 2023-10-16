package com.htmlism.temporaldiagrams.v2

import weaver.*

import com.htmlism.temporaldiagrams.v2.ToyDiagramLanguage.*

object BrightEncoderSuite extends FunSuite {
  test("A bright encoder's default encoding is equal to its bright encoding") {
    val enc =
      new BrightEncoder[ToyDiagramLanguage, Amazon.Ec2] {
        def encodeBrightly(x: Amazon.Ec2, isBright: Boolean): ToyDiagramLanguage =
          ToyDiagramLanguage.Component(isBright.toString)
      }

    val domainInput =
      Amazon.Ec2("")

    expect.eql(
      enc.encode(domainInput),
      enc.encodeBrightly(domainInput, isBright = true)
    ) and expect.eql(
      enc.encodeWithHighlights(domainInput, highlighted = true),
      enc.encodeBrightly(domainInput, isBright          = true)
    )
  }
}
