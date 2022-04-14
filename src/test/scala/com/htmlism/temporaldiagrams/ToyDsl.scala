package com.htmlism.temporaldiagrams

import cats.syntax.all._

sealed trait ToyDsl

case class Service(name: String, dependency: Option[Service]) extends ToyDsl

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
          x.dependency.map(y => s"${y.name} --> ${x.name}").toList

        ((component + tagStr) :: dependency)
          .mkString(joiner)
      }

      def encode(r: Renderable[Service]): String = {
        r match {
          case ConstantRenderable(x) =>
            renderFlat(x, None)

          case IdentifiedRenderable(_, x) =>
            renderFlat(x, None)

          case RenderableCons(x, y) =>
            encode(x) + joiner + encode(y)
        }
      }

      def encodeWithHighlights(r: Renderable[Service], highlights: Set[String]): String = {
        r match {
          case ConstantRenderable(x) =>
            renderFlat(x, "Dim".some)

          case IdentifiedRenderable(id, x) =>
            if (highlights(id))
              renderFlat(x, "Highlighted".some)
            else
              renderFlat(x, "Dim".some)

          case RenderableCons(x, y) =>
            encodeWithHighlights(x, highlights) + joiner + encodeWithHighlights(y, highlights)
        }
      }

    }
}
