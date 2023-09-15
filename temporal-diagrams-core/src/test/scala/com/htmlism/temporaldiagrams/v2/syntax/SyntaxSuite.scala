package com.htmlism.temporaldiagrams.v2
package syntax

import weaver._

object SyntaxSuite extends FunSuite {
  case class Foo(s: String)

  object Foo {
    implicit val fooStrEncoder: HighlightEncoder[String, Foo] =
      new HighlightEncoder[String, Foo] {
        def encode(x: Foo): String =
          x.s

        def encodeWithHighlights(x: Foo, highlighted: Boolean): String =
          x.s + highlighted.toString
      }
  }

  case class Bar(s: String)

  object Bar {
    implicit val barStrEncoder: HighlightEncoder[String, Bar] =
      new HighlightEncoder[String, Bar] {
        def encode(x: Bar): String =
          x.s

        def encodeWithHighlights(x: Bar, highlighted: Boolean): String =
          x.s + highlighted.toString
      }
  }

  test("Domain objects from unrelated hierarchies can be bound together, implicitly") {
    val _ =
      Renderable[String](
        Foo(""),
        Bar("")
      )

    expect.eql(1, 1)
  }
}
