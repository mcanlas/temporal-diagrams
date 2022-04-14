package com.htmlism.temporaldiagrams

sealed trait ToyDsl

case class Service(name: String, dependency: Option[Service]) extends ToyDsl

object Service {
  implicit val servicePlantUmlEncoder: DslEncoder[Service, PlantUml] =
    new DslEncoder[Service, PlantUml] {
      def encode(r: Renderable[Service]): String =
        r match {
          case ConstantRenderable(x) =>
            s"component ${x.name}"

          case IdentifiedRenderable(id, x) =>
            s"component ${x.name}"

          case RenderableCons(x, y) =>
            ???
          // TODO maybe a canvas-powered plus goes here
          //          render[B](x) + "\n" + render[B](y)
        }

    }
}
