package com.htmlism.temporaldiagrams

import cats.syntax.all._
import weaver._

import com.htmlism.temporaldiagrams.syntax._

object HighlightSuite extends FunSuite {
  import PlantUml._

  test("Two objects with no highlights should render dimly") {
    val foo =
      Service("foo")

    val bar =
      Service("bar", "foo".some)

    val withTags =
      List(foo.tag("foo"), bar.tag("bar"))

    expect.same(
      DslEncoder.encodeManyWithHighlights[Service, PlantUml](withTags),
      List(
        Component("foo"),
        Component("bar"),
        Link("foo", "bar")
      )
    )
  }

  test("Two objects with one highlight should highlight one and dim the other") {
    val foo =
      Service("foo")

    val bar =
      Service("bar", "foo".some)

    val withTags =
      List(foo.tag("foo"), bar.tag("bar"))

    expect.same(
      DslEncoder.encodeManyWithHighlights[Service, PlantUml](withTags, "foo"),
      List(
        Component("foo") of "Service",
        Component("bar"),
        Link("foo", "bar")
      )
    )
  }

  test("Two objects with multiple highlights should highlight everything") {
    val foo =
      Service("foo")

    val bar =
      Service("bar", "foo".some)

    val withTags =
      List(foo.tag("foo"), bar.tag("bar"))

    expect.same(
      DslEncoder.encodeManyWithHighlights[Service, PlantUml](
        withTags,
        "foo",
        "bar"
      ),
      List(
        Component("foo") of "Service",
        Component("bar") of "Service",
        Link("foo", "bar")
      )
    )
  }
}
