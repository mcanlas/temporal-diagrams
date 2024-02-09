package com.htmlism.temporaldiagrams

import cats.data.*
import cats.syntax.all.*
import weaver.*

import com.htmlism.temporaldiagrams.ToyDiagramLanguage.*

object HighlightEncoderSuite extends FunSuite:
  test("A diagram encoder can encode"):
    expect.eql(
      Chain:
        Component("amazon ec2: abc")
      ,
      summon[HighlightEncoder[Chain[ToyDiagramLanguage], Amazon.Ec2]].encode(Amazon.Ec2("abc"))
    )

  test("A diagram encoder can encode with highlights"):
    expect.eql(
      Chain:
        Component("amazon ec2: abc true")
      ,
      summon[HighlightEncoder[Chain[ToyDiagramLanguage], Amazon.Ec2]]
        .encodeWithHighlights(
          Amazon.Ec2("abc"),
          highlighted = true
        )
    )

  test("A diagram encoder is contravariant"):
    val repeatEncoder =
      summon[HighlightEncoder[Chain[ToyDiagramLanguage], Amazon.Ec2]].contramap((s: String) => Amazon.Ec2(s"$s $s"))

    expect.eql(
      Chain:
        Component("amazon ec2: twin twin")
      ,
      repeatEncoder.encode("twin")
    ) and expect.eql(
      Chain:
        Component("amazon ec2: twin twin true")
      ,
      repeatEncoder.encodeWithHighlights("twin", highlighted = true)
    )
