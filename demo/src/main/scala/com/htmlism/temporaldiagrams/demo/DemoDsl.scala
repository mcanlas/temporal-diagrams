package com.htmlism.temporaldiagrams
package demo

import cats.syntax.all._

sealed trait DemoDsl

case class Service(name: String, dependency: Option[String]) extends DemoDsl

object Service {
  implicit val servicePlantUmlEncoder: DslEncoder[Service, PlantUml] =
    new DslEncoder[Service, PlantUml] {
      def encodeWithHighlights(r: Renderable[Service], highlights: Set[String]): List[PlantUml] =
        r match {
          case Renderable.Anonymous(x) =>
            renderFlatMonoid(x, "Dim".some)

          case Renderable.ById(id, x) =>
            if (highlights(id))
              renderFlatMonoid(x, "Highlighted".some)
            else
              renderFlatMonoid(x, "Dim".some)

          case Renderable.Cons(x, y) =>
            encodeWithHighlights(x, highlights) ::: encodeWithHighlights(y, highlights)
        }

      def encode(x: Renderable[Service]): List[PlantUml] =
        x match {
          case Renderable.Anonymous(x) =>
            renderFlatMonoid(x, None)

          case Renderable.ById(_, x) =>
            renderFlatMonoid(x, None)

          case Renderable.Cons(x, y) =>
            encode(x) ::: encode(y)
        }

      private def renderFlatMonoid(x: Service, tag: Option[String]) = {
        val component =
          List(Component(x.name, tag))

        val dependency =
          x.dependency.toList.map(Link(_, x.name))

        component ::: dependency
      }
    }
}
