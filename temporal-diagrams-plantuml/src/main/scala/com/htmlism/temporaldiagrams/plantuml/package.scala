package com.htmlism.temporaldiagrams

package object plantuml {
  implicit class ApplyWhenOps[A](x: A) {
    def applyWhen[AA >: A, B](ob: Option[B])(f: (A, B) => AA): AA =
      ob
        .map(f(x, _))
        .getOrElse(x)
  }
}
