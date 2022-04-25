package com.htmlism.temporaldiagrams

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import com.htmlism.temporaldiagrams.syntax._

class NarrativeSpec extends AnyFlatSpec with Inside with Matchers {
  it should "support progressive building by prepending" in {
    val services =
      Nel
        .of(Service("foo", None), Service("bar", None))
        .map(_.r.f[Int])

    val narrative =
      services
        .start
        .next("foo" -> 1)
        .next("foo" -> 2)

    narrative.episodeSelectors shouldBe Nel.of(
      Nil,
      List("foo" -> 1),
      List("foo" -> 2, "foo" -> 1)
    )
  }

  it should "support resetting" in {
    val services =
      Nel
        .of(Service("foo", None), Service("bar", None))
        .map(_.r.f[Int])

    val narrative =
      services
        .start
        .next("foo" -> 1)
        .reset("foo" -> 2)

    narrative.episodeSelectors shouldBe Nel.of(
      Nil,
      List("foo" -> 1),
      List("foo" -> 2)
    )
  }

  it should "support mass frame selection" in {
    val fooVariants =
      FacetedFrame
        .from("foo", "first" -> Service("foo", None).r, "second" -> Service("newfoo", None).r)

    val barVariants =
      FacetedFrame
        .from("bar", "first" -> Service("bar", None).r, "second" -> Service("newbar", None).r)

    val narrative =
      Nel
        .of(fooVariants, barVariants)
        .start
        .next("foo" -> "second")
        .next("bar" -> "second")

    val episodes =
      narrative.episodes.toList

    episodes(0) shouldBe Nel.of(Service("foo", None).r, Service("bar", None).r)
    episodes(1) shouldBe Nel.of(Service("newfoo", None).r, Service("bar", None).r)
    episodes(2) shouldBe Nel.of(Service("newfoo", None).r, Service("newbar", None).r)
  }
}
