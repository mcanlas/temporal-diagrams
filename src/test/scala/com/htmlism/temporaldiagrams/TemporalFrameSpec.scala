package com.htmlism.temporaldiagrams

import cats.syntax.all._
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import com.htmlism.temporaldiagrams.syntax._

class TemporalFrameSpec extends AnyFlatSpec with Inside with Matchers {
  "Temporal DSL" should "not react when fixed" in {
    val temporal =
      Service("foo1", None).r.t[Int]

    temporal.at(1) shouldBe temporal.at(2)
  }

  it should "access by key directly" in {
    val one =
      Service("foo1", None).r

    val two =
      Service("foo2", None).r

    val temporal =
      TemporalFrame(
        1 -> one,
        2 -> two
      )

    temporal.at(1) shouldBe one
  }

  it should "access by key less than" in {
    val one =
      Service("foo1", None).r

    val two =
      Service("foo2", None).r

    val temporal =
      TemporalFrame(
        1 -> one,
        2 -> two
      )

    temporal.at(0) shouldBe one
  }

  it should "access by key more than" in {
    val one =
      Service("foo1", None).r

    val two =
      Service("foo2", None).r

    val temporal =
      TemporalFrame(
        1 -> one,
        2 -> two
      )

    temporal.at(3) shouldBe two
  }

  it should "pick the lower when in between values" in {
    val one =
      Service("foo1", None).r

    val three =
      Service("foo3", None).r

    val temporal =
      TemporalFrame(
        1 -> one,
        3 -> three
      )

    temporal.at(2) shouldBe one
  }

  it should "support cons" in {
    val leftTemporal =
      TemporalFrame(
        1 -> Service("foo1", None).r,
        3 -> Service("foo3", None).r
      )

    val rightTemporal =
      TemporalFrame(
        2 -> Service("foo2", None).r,
        4 -> Service("foo4", None).r
      )

    val atTwo =
      (leftTemporal |+| rightTemporal)
        .at(2)

    inside(atTwo) { case Renderable.Cons(x, y) =>
      x shouldBe Service("foo1", None).r
      y shouldBe Service("foo2", None).r
    }
  }

  it should "collect keys" in {
    val leftTemporal =
      TemporalFrame(
        1 -> Service("foo1", None).r,
        3 -> Service("foo3", None).r
      )

    val rightTemporal =
      TemporalFrame(
        2 -> Service("foo2", None).r,
        4 -> Service("foo4", None).r
      )

    (leftTemporal |+| rightTemporal).keys should contain theSameElementsAs List(1, 2, 3, 4)
  }
}
