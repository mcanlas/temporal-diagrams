package com.htmlism.temporaldiagrams

import cats.syntax.all._
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import com.htmlism.temporaldiagrams.syntax._

class HighlightSpec extends AnyFlatSpec with Inside with Matchers {
  "Two objects with no highlights" should "render dimly" in {
    val foo =
      Service("foo", None)

    val bar =
      Service("bar", "foo".some)

    (foo.id("foo") |+| bar.id("bar"))
      .renderWithHighlightsOn() shouldBe "component foo << Dim >>\n\ncomponent bar << Dim >>\n\nfoo --> bar"
  }

  "Two objects with one highlight" should "highlight one and dim the other" in {
    val foo =
      Service("foo", None)

    val bar =
      Service("bar", "foo".some)

    (foo.id("foo") |+| bar.id("bar"))
      .renderWithHighlightsOn(
        "foo"
      ) shouldBe "component foo << Highlighted >>\n\ncomponent bar << Dim >>\n\nfoo --> bar"
  }

  "Two objects with multiple highlights" should "highlight everything" in {
    val foo =
      Service("foo", None)

    val bar =
      Service("bar", "foo".some)

    (foo.id("foo") |+| bar.id("bar"))
      .renderWithHighlightsOn(
        "foo",
        "bar"
      ) shouldBe "component foo << Highlighted >>\n\ncomponent bar << Highlighted >>\n\nfoo --> bar"
  }
}
