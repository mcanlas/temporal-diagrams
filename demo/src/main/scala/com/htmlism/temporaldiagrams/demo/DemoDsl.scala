package com.htmlism.temporaldiagrams
package demo

import cats.syntax.all._

sealed trait DemoDsl

case class Service(name: String, dependency: Option[String]) extends DemoDsl
case class Hydra(name: String, dependency: Option[String]) extends DemoDsl
case class Buffered(name: String, dependency: Option[String]) extends DemoDsl

object DemoDsl {
  import PlantUml._

  private val spotlightStyle =
    """
      |skinparam component {
      |  fontStyle bold
      |  fontColor #AAA
      |  backgroundColor white
      |  borderColor #AAA
      |  borderThickness 2
      |}
      |
      |skinparam queue {
      |  fontStyle bold
      |  fontColor #AAA
      |  backgroundColor white
      |  borderColor #AAA
      |  borderThickness 2
      |}
      |
      |skinparam database {
      |  fontStyle bold
      |  fontColor #AAA
      |  backgroundColor white
      |  borderColor #AAA
      |  borderThickness 2
      |}
      |
      |skinparam component<< Service >> {
      |  fontStyle bold
      |  fontColor white
      |  backgroundColor #586ba4
      |  borderColor #223336
      |  borderThickness 2
      |}""".stripMargin

  implicit val servicePlantUmlEncoder: DslEncoder[DemoDsl, PlantUml] =
    new DslEncoder[DemoDsl, PlantUml] {
      def injectedStyle: String =
        spotlightStyle

      def encodeWithHighlights(r: Renderable[DemoDsl], highlights: Set[String]): List[PlantUml] =
        r match {
          case Renderable.Anonymous(x) =>
            renderFlatMonoid(x, brightly = false)

          case Renderable.ById(id, x) =>
            if (highlights(id))
              renderFlatMonoid(x, brightly = true)
            else
              renderFlatMonoid(x, brightly = false)

          case Renderable.Cons(x, y) =>
            encodeWithHighlights(x, highlights) ::: encodeWithHighlights(y, highlights)
        }

      def encode(x: Renderable[DemoDsl]): List[PlantUml] =
        x match {
          case Renderable.Anonymous(x) =>
            renderFlatMonoid(x, brightly = true)

          case Renderable.ById(_, x) =>
            renderFlatMonoid(x, brightly = true)

          case Renderable.Cons(x, y) =>
            encode(x) ::: encode(y)
        }

      private def renderFlatMonoid(x: DemoDsl, brightly: Boolean) =
        x match {
          case Service(name, dependency) =>
            val component =
              List(Component(name, None, if (brightly) "Service".some else None))

            val link =
              dependency.toList.map(Link(_, name))

            component ::: link

          case Hydra(name, dependency) =>
            (1 to 3)
              .flatMap(n => List(Component(name + n.toString, None, if (brightly) "Service".some else None)) ++ dependency.toList.map(Link(_, name + n.toString)))
              .toList

          case Buffered(name, dependency) =>
            List(
              Component(name, None, if (brightly) "Service".some else None),
              Queue(name + "_queue", None),
              Link(name + "_queue", name)) ++ dependency.toList.map(Link(_, name + "_queue"))
        }
    }
}
