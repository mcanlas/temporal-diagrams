package com.htmlism.temporaldiagrams

import cats.data.NonEmptyList
import weaver._

object MultiArrowSuite extends FunSuite {
  import PlantUml._

  test("A multiarrow spec should render as a product") {
    val things =
      List(
        Renderable.Source("foo", NonEmptyList.of("foo-1", "foo-2")),
        Renderable.Destination("bar", NonEmptyList.of("bar-1", "bar-2")),
        Renderable.MultiArrow("foo", "bar")
      )

    expect.same(
      DslEncoder.encodeMany[Service, PlantUml](things),
      List(
        Component("foo-1 to bar-1", None, Some("Service")),
        Component("foo-1 to bar-2", None, Some("Service")),
        Component("foo-2 to bar-1", None, Some("Service")),
        Component("foo-2 to bar-2", None, Some("Service"))
      )
    )
  }
}
