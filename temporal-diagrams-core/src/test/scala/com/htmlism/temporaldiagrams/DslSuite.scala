package com.htmlism.temporaldiagrams

import weaver.*

import com.htmlism.temporaldiagrams.syntax.*

object DslSuite extends FunSuite:
  import PlantUml.*

  test("support tagging"):
    val foo =
      Service("foo")

    val renderable =
      foo.tag("foo-tag", "bar-tag")

    expect.same(
      Renderable.Tagged(List("foo-tag", "bar-tag"), foo),
      renderable
    )

  test("A nameless DSL object should support simple rendering"):
    val foo =
      Service("foo")

    expect.same(
      List(Component("foo") of "Service"),
      DslEncoder
        .encodeMany[Service, PlantUml](List(foo.r))
    )

  test("A named DSL object should render the same as a nameless one"):
    val foo =
      Service("foo")

    expect.same(
      DslEncoder
        .encodeMany[Service, PlantUml](List(foo.r)),
      DslEncoder
        .encodeMany[Service, PlantUml](List(foo.tag("foo-id")))
    )
