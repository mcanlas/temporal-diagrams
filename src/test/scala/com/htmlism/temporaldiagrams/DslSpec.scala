package com.htmlism.temporaldiagrams

import cats.syntax.all._
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import com.htmlism.temporaldiagrams.dsl._

class DslSpec extends AnyFlatSpec with Inside with Matchers {
  "A given DSL" should "support cons" in {
    val foo =
      Service("foo", None)

    val bar =
      Service("bar", foo.some)

    val together =
      foo.r |+| bar.r

    inside(together) { case RenderableCons(ConstantRenderable(x), ConstantRenderable(y)) =>
      x shouldBe foo
      y shouldBe bar
    }
  }

  it should "support id" in {
    val foo =
      Service("foo", None)

    val id =
      "foo-id"

    val renderable =
      foo.id(id)

    renderable shouldBe IdentifiedRenderable(id, foo)
  }

  "A nameless DSL object" should "support simple rendering" in {
    val foo =
      Service("foo", None)

    foo.r.renderAs[PlantUml] shouldBe "component foo"
  }

  "A named DSL object" should "render the same as a nameless one" in {
    val foo =
      Service("foo", None)

    foo.r.renderAs[PlantUml] shouldBe foo.id("foo-id").renderAs[PlantUml]
  }

  "Two cons DSL objects" should "support rendering using dialect cons" in {
    val foo =
      Service("foo", None)

    val bar =
      Service("bar", foo.some)

    (foo.r |+| bar.r).renderAs[PlantUml] shouldBe "component foo\n\ncomponent bar\n\nfoo --> bar"
  }
}
