package com.htmlism.temporaldiagrams
package demo

import cats.syntax.all._

sealed trait DemoDsl

case class Service(name: String, dependency: Option[String]) extends DemoDsl

object Service {
  implicit val servicePlantUmlEncoder: DslEncoder[Service, PlantUml] =
    new DslEncoder[Service, PlantUml] {
      private val joiner =
        implicitly[Dialect[PlantUml]].joiner

      private def renderFlat(x: Service, tag: Option[String]) = {
        val component =
          "component " + x.name

        val tagStr =
          tag.fold("")(s => s" << $s >>")

        val dependency =
          x.dependency.map(y => s"$y --> ${x.name}").toList

        ((component + tagStr) :: dependency)
          .mkString(joiner)
      }

      def encodeWithHighlights(r: Renderable[Service], highlights: Set[String]): String = {
        r match {
          case Renderable.Anonymous(x) =>
            renderFlat(x, "Dim".some)

          case Renderable.ById(id, x) =>
            if (highlights(id))
              renderFlat(x, "Highlighted".some)
            else
              renderFlat(x, "Dim".some)

          case Renderable.Cons(x, y) =>
            encodeWithHighlights(x, highlights) + joiner + encodeWithHighlights(y, highlights)
        }
      }

      def encodeMonoid(x: Renderable[Service]): List[PlantUml] =
        x match {
          case Renderable.Anonymous(x) =>
            renderFlatMonoid(x, None)

          case Renderable.ById(_, x) =>
            renderFlatMonoid(x, None)

          case Renderable.Cons(x, y) =>
            encodeMonoid(x) ::: encodeMonoid(y)
        }

      private def renderFlatMonoid(x: Service, tag: Option[String]) = {
        val _ =
          tag

        val component =
          List(Component(x.name))

        val dependency =
          x.dependency.toList.map(Link(_, x.name))

        component ::: dependency
      }
    }
}
