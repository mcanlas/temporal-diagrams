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
          case Renderable.Empty =>
            Nil

          case Renderable.Tagged(tags, x) =>
            if ((highlights intersect tags.toSet).nonEmpty)
              renderFlatMonoid(x, "Service".some)
            else
              renderFlatMonoid(x, None)

          case Renderable.Cons(x, y) =>
            encodeWithHighlights(x, highlights) ::: encodeWithHighlights(y, highlights)
        }

      def encode(x: Renderable[Service]): List[PlantUml] =
        x match {
          case Renderable.Empty =>
            Nil

          case Renderable.Tagged(_, x) =>
            renderFlatMonoid(x, "Service".some)

          case Renderable.Cons(x, y) =>
            encode(x) ::: encode(y)
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
