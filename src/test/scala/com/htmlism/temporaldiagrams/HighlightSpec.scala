package com.htmlism.temporaldiagrams

import cats.syntax.all._
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import com.htmlism.temporaldiagrams.syntax._

class HighlightSpec extends AnyFlatSpec with Inside with Matchers {
  import PlantUml._

  "Two objects with no highlights" should "render dimly" in {
    val foo =
      Service("foo")

    val bar =
      Service("bar", "foo".some)

    (foo.id("foo") |+| bar.id("bar"))
      .encodeWithHighlightsOn[PlantUml]() should contain theSameElementsAs List(
      Component("foo"),
      Component("bar"),
      Link("foo", "bar")
    )
  }

  "Two objects with one highlight" should "highlight one and dim the other" in {
    val foo =
      Service("foo")

    val bar =
      Service("bar", "foo".some)

    (foo.id("foo") |+| bar.id("bar"))
      .encodeWithHighlightsOn[PlantUml](
        "foo"
      ) should contain theSameElementsAs List(Component("foo") of "Service", Component("bar"), Link("foo", "bar"))
  }

  "Two objects with multiple highlights" should "highlight everything" in {
    val foo =
      Service("foo")

    val bar =
      Service("bar", "foo".some)

    (foo.id("foo") |+| bar.id("bar"))
      .encodeWithHighlightsOn[PlantUml](
        "foo",
        "bar"
      ) should contain theSameElementsAs List(
      Component("foo") of "Service",
      Component("bar") of "Service",
      Link("foo", "bar")
    )
  }
}
