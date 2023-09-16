package com.htmlism.temporaldiagrams

package object plantuml {
  type PlantUmlEncoder[A] = v2.DiagramEncoder[PlantUml, A]

  implicit class ApplyWhenOps[A](x: A) {
    def applyWhen[AA >: A, B](ob: Option[B])(f: (A, B) => AA): AA =
      ob
        .map(f(x, _))
        .getOrElse(x)
  }
}
