package com.htmlism.temporaldiagrams

import cats.data.*
import weaver.*

import com.htmlism.temporaldiagrams.syntax.*

object NarrativeSuite extends FunSuite:
  test("support progressive building by prepending"):
    val services =
      NonEmptyList
        .of(Service("foo", None), Service("bar", None))
        .map(_.r.f[Int])

    val narrative =
      services
        .start
        .next("foo" -> 1)
        .next("foo" -> 2)

    expect.same(
      narrative.episodeSelectors,
      NonEmptyList.of(
        Nil,
        List("foo" -> 1),
        List("foo" -> 2, "foo" -> 1)
      )
    )

  test("support resetting"):
    val services =
      NonEmptyList
        .of(Service("foo", None), Service("bar", None))
        .map(_.r.f[Int])

    val narrative =
      services
        .start
        .next("foo" -> 1)
        .reset("foo" -> 2)

    expect.same(
      narrative.episodeSelectors,
      NonEmptyList.of(
        Nil,
        List("foo" -> 1),
        List("foo" -> 2)
      )
    )

  test("support mass frame selection"):
    val fooVariants =
      FacetedFrame
        .from("foo", "first" -> Service("foo", None).r.list, "second" -> Service("newfoo", None).r.list)

    val barVariants =
      FacetedFrame
        .from("bar", "first" -> Service("bar", None).r.list, "second" -> Service("newbar", None).r.list)

    val narrative =
      NonEmptyList
        .of(fooVariants, barVariants)
        .start
        .next("foo" -> "second")
        .next("bar" -> "second")

    val episodes =
      narrative.episodes.toList

    expect.same(episodes(0), List(Service("foo", None).r, Service("bar", None).r)) and
      expect.same(episodes(1), List(Service("newfoo", None).r, Service("bar", None).r)) and
      expect.same(episodes(2), List(Service("newfoo", None).r, Service("newbar", None).r))
