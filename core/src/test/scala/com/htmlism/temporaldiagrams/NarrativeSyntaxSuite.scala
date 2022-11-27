package com.htmlism.temporaldiagrams

import cats.data._
import weaver._

import com.htmlism.temporaldiagrams.syntax._

object NarrativeSyntaxSuite extends FunSuite {
  test("A narrative should be started from a bundle of frames") {
    val services =
      NonEmptyList
        .of(Service("foo", None), Service("bar", None))
        .map(_.r.f[Int])

    val narrative =
      services.start

    expect.same(narrative.frames, services) and
      expect.same(narrative.episodeSelectors, NonEmptyList.of(Nil))
  }
}
