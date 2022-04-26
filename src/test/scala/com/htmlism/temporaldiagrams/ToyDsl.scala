package com.htmlism.temporaldiagrams

import cats.syntax.all._

sealed trait ToyDsl

case class Service(name: String, dependency: Option[String]) extends ToyDsl

object Service {
  import PlantUml._

  implicit val servicePlantUmlEncoder: DslEncoder[Service, PlantUml] =
    new DslEncoder[Service, PlantUml] {
      def encodeWithHighlights(r: Renderable[Service], highlights: Set[String]): List[PlantUml] =
        r match {
          case Renderable.Tagged(tags, x) =>
            if ((highlights intersect tags.toSet).nonEmpty)
              renderFlatMonoid(x, "Service".some)
            else
              renderFlatMonoid(x, None)
        }

      def encode(x: Renderable[Service]): List[PlantUml] =
        x match {
          case Renderable.Tagged(_, x) =>
            renderFlatMonoid(x, "Service".some)
        }

      private def renderFlatMonoid(x: Service, tag: Option[String]) = {
        val component =
          List(Component(x.name, None, tag))

        val dependency =
          x.dependency.toList.map(Link(_, x.name))

        component ::: dependency
      }
    }

  def apply(s: String): Service =
    Service(s, None)
}
