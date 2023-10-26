package com.htmlism.temporaldiagrams
package demo.v2

import cats.data.Chain

import com.htmlism.temporaldiagrams.plantuml.*
import com.htmlism.temporaldiagrams.v2.BrightEncoder

sealed trait DemoDsl

object DemoDsl:
  case class ClusterService(name: String, dependency: Option[String], asCluster: Boolean) extends DemoDsl
  case class Buffered(name: String, dependency: Option[String])                           extends DemoDsl

  given BrightEncoder[Chain[PlantUml], DemoDsl] with
    def encodeBrightly(x: DemoDsl, isBright: Boolean): Chain[PlantUml] =
      x match
        case ClusterService(n, oDep, asCluster) =>
          if asCluster then
            Chain(1, 2, 3, 4)
              .flatMap { i =>
                Chain[PlantUml](
                  PlantUml.Component(n + i.toString, None, Option.when(isBright)("Service")),
                  skin(isBright)
                )
                  .applySome(oDep) { (a, d) =>
                    a.append(PlantUml.Link(d + i.toString, n, 2, None))
                  }
              }
          else
            Chain[PlantUml](
              PlantUml.Component(n, None, Option.when(isBright)("Service")),
              skin(isBright)
            )
              .applySome(oDep) { (a, d) =>
                a.append(PlantUml.Link(d, n, 2, None))
              }

        case Buffered(n, oDep) =>
          Chain[PlantUml](
            PlantUml.Component(n, None, Option.when(isBright)("Service")),
            skin(isBright),
            PlantUml.Queue(n + "_queue", None, None),
            PlantUml.Link(n + "_queue", n, 2, None),
            queueSkin
          )
            .applySome(oDep) { (a, d) =>
              a.append(PlantUml.Link(d, n + "_queue", 2, None))
            }

  private val queueSkin =
    PlantUml
      .SkinParamGroup("queue")
      .and("fontStyle", "bold")
      .and("fontColor", "#AAA")
      .and("backgroundColor", "white")
      .and("borderColor", "#AAA")
      .and("borderThickness", "2")

  private def skin(isBright: Boolean) =
    if isBright then
      PlantUml
        .SkinParamGroup("component", "Service")
        .and("fontStyle", "bold")
        .and("fontColor", "white")
        .and("backgroundColor", "#586ba4")
        .and("borderColor", "#223336")
        .and("borderThickness", "2")
    else
      PlantUml
        .SkinParamGroup("component")
        .and("fontStyle", "bold")
        .and("fontColor", "#AAA")
        .and("backgroundColor", "white")
        .and("borderColor", "#AAA")
        .and("borderThickness", "2")

  case class ConfigBasket(fooStyle: ConfigBasket.ServiceAppearance, barStyle: ConfigBasket.ServiceAppearance)

  object ConfigBasket:
    sealed trait ServiceAppearance

    object ServiceAppearance:
      case object AsSingleton extends ServiceAppearance

      case object AsCluster extends ServiceAppearance

      case object WithBuffer extends ServiceAppearance
