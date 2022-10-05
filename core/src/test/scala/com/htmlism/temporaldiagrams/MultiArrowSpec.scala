package com.htmlism.temporaldiagrams

import cats.data.NonEmptyList
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class MultiArrowSpec extends AnyFlatSpec with Inside with Matchers {
  import PlantUml._

  "A multiarrow spec" should "render as a product" in {
    val things =
      List(
        Renderable.Source("foo", NonEmptyList.of("foo-1", "foo-2")),
        Renderable.Destination("bar", NonEmptyList.of("bar-1", "bar-2")),
        Renderable.MultiArrow("foo", "bar")
      )

    DslEncoder.encodeMany[Service, PlantUml](things) should contain theSameElementsAs List(
      Component("foo-1 to bar-1", None, Some("Service")),
      Component("foo-1 to bar-2", None, Some("Service")),
      Component("foo-2 to bar-1", None, Some("Service")),
      Component("foo-2 to bar-2", None, Some("Service"))
    )
  }
}
