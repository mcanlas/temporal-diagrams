package com.htmlism.temporaldiagrams

import weaver.*

import com.htmlism.temporaldiagrams.syntax.*

object FacetedFrameSyntaxSuite extends FunSuite {
  test("Faceted frames DSL should support building one") {
    val component =
      Service("foo1").r

    matches(component.f) { case FacetedFrame.Fixed(xs) =>
      expect.same(xs, List(component))
    }
  }
}
