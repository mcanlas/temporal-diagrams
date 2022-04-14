package com.htmlism.temporaldiagrams

sealed trait ToyDsl

case class Service(name: String, dependency: Option[Service]) extends ToyDsl

object Service {
  implicit val servicePlantUmlEncoder: DslEncoder[Service, PlantUml] =
    new DslEncoder[Service, PlantUml] {
      private val joiner =
        implicitly[Dialect[PlantUml]].joiner

      private def renderFlat(x: Service) = {
        val component =
          "component " + x.name

        val dependency =
          x.dependency.map(y => s"${y.name} --> ${x.name}").toList

        (component :: dependency)
          .mkString(joiner)
      }

      def encode(r: Renderable[Service]): String = {
        r match {
          case ConstantRenderable(x) =>
            renderFlat(x)

          case IdentifiedRenderable(_, x) =>
            renderFlat(x)

          case RenderableCons(x, y) =>
            encode(x) + joiner + encode(y)
        }
      }

    }
}
