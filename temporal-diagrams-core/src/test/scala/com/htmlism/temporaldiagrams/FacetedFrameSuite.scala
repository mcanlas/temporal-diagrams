package com.htmlism.temporaldiagrams

import cats.data.*
import weaver.*

import com.htmlism.temporaldiagrams.syntax.*

object FacetedFrameSuite extends FunSuite:
  test("Faceted frames should support building many"):
    val component =
      Service("foo1").r

    val component2 =
      Service("foo2").r

    val frame =
      FacetedFrame
        .from("some id", "key1" -> component.list, "key2" -> component2.list)

    matches(frame) { case FacetedFrame.WithKeys(id, xs) =>
      expect.eql(id, "some id") and
        expect.same(xs.head, "key1" -> component.list) and
        expect.same(xs.tail.head, "key2" -> component2.list)
    }

  test("support building one"):
    val component =
      Service("foo1").r

    val frame =
      FacetedFrame
        .fixed(component.list)

    matches(frame) { case FacetedFrame.Fixed(x) =>
      expect.same(x, component.list)
    }

  test("Frame selection pick a variant by key"):
    val default =
      Service("default").r

    val variant =
      Service("variant").r

    val frame =
      FacetedFrame
        .from("some id", "key1" -> default.list, "key2" -> variant.list)

    expect.same(
      FacetedFrame
        .selectFrames(NonEmptyList.of(frame), "some id" -> "key2"),
      variant.list
    )

  test("pick a default if selectors don't match"):
    val default =
      Service("default").r

    val variant =
      Service("variant").r

    val frame =
      FacetedFrame
        .from("some id", "key1" -> default.list, "key2" -> variant.list)

    expect.same(
      FacetedFrame
        .selectFrames(NonEmptyList.of(frame), "nonsense id" -> "key1"),
      default.list
    ) and expect.same(
      FacetedFrame
        .selectFrames(NonEmptyList.of(frame), "some id" -> "nonsense key"),
      default.list
    )

  test("keep the number of input frames the same as the output"):
    val services =
      List(Service("default").r, Service("variant").r)

    expect.same(
      FacetedFrame
        .selectFrames(FacetedFrame.fixed(services).nel),
      services
    )
