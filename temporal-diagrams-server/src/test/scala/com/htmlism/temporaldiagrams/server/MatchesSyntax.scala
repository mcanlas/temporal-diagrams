package com.htmlism.temporaldiagrams.server

import cats.Show
import weaver._

trait MatchesSyntax {
  self: Expectations.Helpers =>

  def matches[A](x: A)(
      f: PartialFunction[A, Expectations]
  )(implicit pos: SourceLocation, A: Show[A] = Show.fromToString[A]): Expectations =
    if (f.isDefinedAt(x))
      f(x)
    else
      failure("Pattern did not match, got: " + A.show(x))
}
