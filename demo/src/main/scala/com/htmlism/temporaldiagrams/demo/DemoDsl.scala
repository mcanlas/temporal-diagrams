package com.htmlism.temporaldiagrams
package demo

import cats.syntax.all._

sealed trait DemoDsl

case class Service(name: String, dependency: Option[String]) extends DemoDsl
case class Hydra(name: String, dependency: Option[String]) extends DemoDsl
case class Buffered(name: String, dependency: Option[String]) extends DemoDsl

object DemoDsl {
  implicit val servicePlantUmlEncoder: DslEncoder[DemoDsl, PlantUml] =
    new DslEncoder[DemoDsl, PlantUml] {
      def encodeWithHighlights(r: Renderable[DemoDsl], highlights: Set[String]): List[PlantUml] =
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

      def encode(x: Renderable[DemoDsl]): List[PlantUml] =
        x match {
          case Renderable.Anonymous(x) =>
            renderFlatMonoid(x, None)

          case Renderable.ById(_, x) =>
            renderFlatMonoid(x, None)

          case Renderable.Cons(x, y) =>
            encode(x) ::: encode(y)
        }

      private def renderFlatMonoid(x: DemoDsl, tag: Option[String]) =
        x match {
          case Service(name, dependency) =>
            val component =
              List(Component(name, tag))

            val link =
              dependency.toList.map(Link(_, name))

            component ::: link

          case Hydra(name, dependency) =>
            (1 to 3)
              .flatMap(n => List(Component(name + n.toString, tag)) ++ dependency.toList.map(Link(_, name + n.toString)))
              .toList

          case Buffered(name, dependency) =>
            List(
              Component(name, tag),
              Queue(name + "_queue", tag),
              Link(name + "_queue", name)) ++ dependency.toList.map(Link(_, name + "_queue"))
        }
    }
}
