package com.htmlism.temporaldiagrams

import cats.data._
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import com.htmlism.temporaldiagrams.syntax._

class FacetedFrameSpec extends AnyFlatSpec with Inside with Matchers {
  "Faceted frames" should "support building many" in {
    val component =
      Service("foo1").r

    val component2 =
      Service("foo2").r

    val frame =
      FacetedFrame
        .from("some id", "key1" -> component, "key2" -> component2)

    inside(frame) { case FacetedFrame.WithKeys(id, xs) =>
      id shouldBe "some id"

      xs.head shouldBe "key1" -> component

      xs.tail.head shouldBe "key2" -> component2
    }
  }

  it should "support building one" in {
    val component =
      Service("foo1").r

    val frame =
      FacetedFrame
        .fixed(component)

    inside(frame) { case FacetedFrame.Fixed(x) =>
      x shouldBe component
    }
  }

  "Frame selection" should "pick a variant by key" in {
    val default =
      Service("default").r

    val variant =
      Service("variant").r

    val frame =
      FacetedFrame
        .from("some id", "key1" -> default, "key2" -> variant)

    FacetedFrame
      .selectFrames(NonEmptyList.of(frame), "some id" -> "key2") shouldBe NonEmptyList.of(variant)
  }

  it should "pick a default if selectors don't match" in {
    val default =
      Service("default").r

    val variant =
      Service("variant").r

    val frame =
      FacetedFrame
        .from("some id", "key1" -> default, "key2" -> variant)

    FacetedFrame
      .selectFrames(NonEmptyList.of(frame), "nonsense id" -> "key1") shouldBe NonEmptyList.of(default)

    FacetedFrame
      .selectFrames(NonEmptyList.of(frame), "some id" -> "nonsense key") shouldBe NonEmptyList.of(default)
  }

  it should "keep the number of input frames the same as the output" in {
    val services =
      NonEmptyList
        .of(Service("default").r, Service("variant").r)

    FacetedFrame
      .selectFrames(services.map(FacetedFrame.fixed)) shouldBe services
  }
}
