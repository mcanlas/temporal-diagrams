package com.htmlism.temporaldiagrams

import weaver._

import com.htmlism.temporaldiagrams.syntax._

object FacetedFrameSyntaxSuite extends FunSuite {
  test("Faceted frames DSL should support building one") {
    val component =
      Service("foo1").r

    matches(component.f) { case FacetedFrame.Fixed(xs) =>
      expect.same(xs, List(component))
    }
  }
}
