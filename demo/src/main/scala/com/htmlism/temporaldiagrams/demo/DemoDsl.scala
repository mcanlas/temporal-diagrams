package com.htmlism.temporaldiagrams
package demo

sealed trait DemoDsl

case class Service(name: String, dependency: Option[String]) extends DemoDsl
case class Hydra(name: String, dependency: Option[String]) extends DemoDsl
case class Buffered(name: String, dependency: Option[String]) extends DemoDsl

object DemoDsl {
  import PlantUml._

  implicit val servicePlantUmlEncoder: DslEncoder[DemoDsl, PlantUml] =
    new DslEncoder[DemoDsl, PlantUml] {
      def encodeWithHighlights(x: DemoDsl, highlighted: Boolean): List[PlantUml] =
        if (highlighted)
          renderFlatMonoid(x, brightly = true)
        else
          renderFlatMonoid(x, brightly = false)

      def encode(x: DemoDsl): List[PlantUml] =
        renderFlatMonoid(x, brightly = true)

      private def skin(brightly: Boolean) =
        if (brightly)
          PlantUml
            .SkinParam
            .build("component", "Service")
            .and("fontStyle", "bold")
            .and("fontColor", "white")
            .and("backgroundColor", "#586ba4")
            .and("borderColor", "#223336")
            .and("borderThickness", "2")
        else
          PlantUml
            .SkinParam
            .build("component")
            .and("fontStyle", "bold")
            .and("fontColor", "#AAA")
            .and("backgroundColor", "white")
            .and("borderColor", "#AAA")
            .and("borderThickness", "2")

      private def renderFlatMonoid(x: DemoDsl, brightly: Boolean) =
        x match {
          case Service(name, dependency) =>
            val component =
              List(Component(name, None, Option.when(brightly)("Service")))

            val link =
              dependency.toList.map(Link(_, name))

            skin(brightly) :: component ::: link

          case Hydra(name, dependency) =>
            (1 to 3)
              .flatMap(n =>
                List(Component(name + n.toString, None, Option.when(brightly)("Service"))) ++ dependency
                  .toList
                  .map(Link(_, name + n.toString))
              )
              .toList appended skin(brightly)

          case Buffered(name, dependency) =>
            List(
              skin(brightly),
              PlantUml
                .SkinParam
                .build("queue")
                .and("fontStyle", "bold")
                .and("fontColor", "#AAA")
                .and("backgroundColor", "white")
                .and("borderColor", "#AAA")
                .and("borderThickness", "2"),
              Component(name, None, Option.when(brightly)("Service")),
              Queue(name + "_queue", None, None),
              Link(name + "_queue", name)
            ) ++ dependency.toList.map(Link(_, name + "_queue"))
        }
    }
}
