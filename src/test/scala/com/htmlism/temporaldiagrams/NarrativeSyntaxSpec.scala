package com.htmlism.temporaldiagrams

import cats.data._
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import com.htmlism.temporaldiagrams.syntax._

class NarrativeSyntaxSpec extends AnyFlatSpec with Inside with Matchers with NonEmptyListAggregating {
  "A narrative" should "be started from a bundle of frames" in {
    val services =
      NonEmptyList
        .of(Service("foo", None), Service("bar", None))
        .map(_.r.f[Int])

    val narrative =
      services.start

    narrative.frames shouldBe services

    narrative.episodeSelectors should contain theSameElementsAs List(Nil)
  }
}
