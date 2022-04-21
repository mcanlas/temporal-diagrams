package com.htmlism.temporaldiagrams

import cats.syntax.all._
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import com.htmlism.temporaldiagrams.syntax._

class DslSpec extends AnyFlatSpec with Inside with Matchers {
  import PlantUml._

  "A given DSL" should "support cons" in {
    val foo =
      Service("foo")

    val bar =
      Service("bar", "foo".some)

    val together =
      foo.r |+| bar.r

    inside(together) { case Renderable.Cons(Renderable.Tagged(_, x), Renderable.Tagged(_, y)) =>
      x shouldBe foo
      y shouldBe bar
    }
  }

  it should "support tagging" in {
    val foo =
      Service("foo")

    val renderable =
      foo.tag("foo-tag", "bar-tag")

    renderable shouldBe Renderable.Tagged(List("foo-tag", "bar-tag"), foo)
  }

  "A nameless DSL object" should "support simple rendering" in {
    val foo =
      Service("foo")

    foo.r.encodeAs[PlantUml] should contain theSameElementsAs List(Component("foo") of "Service")
  }

  "A named DSL object" should "render the same as a nameless one" in {
    val foo =
      Service("foo")

    foo.r.encodeAs[PlantUml] should contain theSameElementsAs foo.tag("foo-id").encodeAs[PlantUml]
  }

  "Two cons DSL objects" should "support rendering using dialect cons" in {
    val foo =
      Service("foo")

    val bar =
      Service("bar", "foo".some)

    (foo.r |+| bar.r).encodeAs[PlantUml] should contain theSameElementsAs List(
      Component("foo") of "Service",
      Component("bar") of "Service",
      Link("foo", "bar")
    )
  }
}
