package com.htmlism.temporaldiagrams.plantuml

import scala.util.chaining._

import cats.data.NonEmptyList

import com.htmlism.temporaldiagrams.v2._

/**
  * A building block in component diagrams
  *
  * @param name
  *   This text is visible in the diagram
  * @param alias
  *   Optional. If provided, arrows can refer to this component by this alias. If not provided, the alias will be the
  *   value of `name`
  */
case class Component(name: String, alias: Option[String])

object Component {
  implicit class ApplyWhenOps[A](x: A) {
    def applyWhen[AA >: A, B](ob: Option[B])(f: (A, B) => AA): AA =
      ob
        .map(f(x, _))
        .getOrElse(x)
  }

  implicit val componentEncoder: BrightEncoder[PlantUml, Component] =
    new BrightEncoder[PlantUml, Component] {
      def bright(x: Component): NonEmptyList[String] = {
        val Component(name, alias) = x

        s"component $name"
          .applyWhen(alias)((s, a) => s + s" as $a")
          .pipe(NonEmptyList.one)
      }

      def dim(x: Component): NonEmptyList[String] =
        NonEmptyList.one("")
    }
}
