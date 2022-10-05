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
        .from("some id", "key1" -> component.list, "key2" -> component2.list)

    inside(frame) { case FacetedFrame.WithKeys(id, xs) =>
      id shouldBe "some id"

      xs.head shouldBe "key1" -> component.list

      xs.tail.head shouldBe "key2" -> component2.list
    }
  }

  it should "support building one" in {
    val component =
      Service("foo1").r

    val frame =
      FacetedFrame
        .fixed(component.list)

    inside(frame) { case FacetedFrame.Fixed(x) =>
      x shouldBe component.list
    }
  }

  "Frame selection" should "pick a variant by key" in {
    val default =
      Service("default").r

    val variant =
      Service("variant").r

    val frame =
      FacetedFrame
        .from("some id", "key1" -> default.list, "key2" -> variant.list)

    FacetedFrame
      .selectFrames(NonEmptyList.of(frame), "some id" -> "key2") should contain theSameElementsAs variant.list
  }

  it should "pick a default if selectors don't match" in {
    val default =
      Service("default").r

    val variant =
      Service("variant").r

    val frame =
      FacetedFrame
        .from("some id", "key1" -> default.list, "key2" -> variant.list)

    FacetedFrame
      .selectFrames(NonEmptyList.of(frame), "nonsense id" -> "key1") should contain theSameElementsAs default.list

    FacetedFrame
      .selectFrames(NonEmptyList.of(frame), "some id" -> "nonsense key") should contain theSameElementsAs default.list
  }

  it should "keep the number of input frames the same as the output" in {
    val services =
      List(Service("default").r, Service("variant").r)

    FacetedFrame
      .selectFrames(FacetedFrame.fixed(services).nel) shouldBe services
  }
}
