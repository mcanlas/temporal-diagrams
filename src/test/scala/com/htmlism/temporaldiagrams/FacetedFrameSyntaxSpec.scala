package com.htmlism.temporaldiagrams

import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import com.htmlism.temporaldiagrams.syntax._

class FacetedFrameSyntaxSpec extends AnyFlatSpec with Inside with Matchers {
  "Faceted frames DSL" should "support building one" in {
    val component =
      Service("foo1").r

    inside(component.f) { case FacetedFrame.Fixed(xs) =>
      xs should contain theSameElementsAs List(component)
    }
  }
}
