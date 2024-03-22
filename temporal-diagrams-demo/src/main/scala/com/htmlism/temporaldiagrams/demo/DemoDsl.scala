package com.htmlism.temporaldiagrams
package demo

import scala.util.chaining.*

import cats.data.Chain
import cats.data.NonEmptyList

import com.htmlism.temporaldiagrams.mermaid.*
import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl.*
import com.htmlism.temporaldiagrams.plantuml.*

sealed trait DemoDsl

object DemoDsl:
  case class ClusterService(name: String, asCluster: Boolean) extends DemoDsl
  case class Buffered(name: String)                           extends DemoDsl
  case class Title(s: String)                                 extends DemoDsl

  case class Arrow(src: String, dest: String)

  object Arrow:
    given MultiArrowEncoder[String, Arrow] with
      def encodeArrow(src: String, dest: String): Arrow =
        Arrow(src, dest)

    given BrightEncoder[PlantUml.ComponentDiagram, Arrow] with
      def encodeBrightly(x: Arrow, isBright: Boolean): PlantUml.ComponentDiagram =
        val Arrow(src, dest) = x

        PlantUml.ComponentDiagram:
          PlantUml.Link(src, dest)

    given BrightEncoder[MermaidDiagram[Flowchart], Arrow] with
      def encodeBrightly(x: Arrow, isBright: Boolean): MermaidDiagram[Flowchart] =
        val Arrow(src, dest) = x

        MermaidDiagram(
          Chain.empty,
          Flowchart(
            Link.LinkChain(
              NonEmptyList.one(src),
              NonEmptyList.of(
                Link
                  .Segment
                  .Visible(
                    1,
                    Link.Weight.Normal,
                    Link.Direction.Single(Link.Head.Arrow),
                    text = None,
                    NonEmptyList.one(dest),
                    style = None
                  )
              )
            )
          )
        )

  given BrightEncoder[PlantUml.ComponentDiagram, DemoDsl] with
    def encodeBrightly(x: DemoDsl, isBright: Boolean): PlantUml.ComponentDiagram =
      x match
        case ClusterService(n, asCluster) =>
          if asCluster then
            Chain(1, 2, 3, 4)
              .flatMap { i =>
                Chain[PlantUml](
                  PlantUml.Component(n + i.toString, None, Option.when(isBright)("Service")),
                  skin(isBright)
                )
              }
              .pipe(PlantUml.ComponentDiagram.apply(_))
          else
            Chain[PlantUml](
              PlantUml.Component(n, None, Option.when(isBright)("Service")),
              skin(isBright)
            )
              .pipe(PlantUml.ComponentDiagram.apply(_))

        case Buffered(n) =>
          Chain[PlantUml](
            PlantUml.Component(n, None, Option.when(isBright)("Service")),
            skin(isBright),
            PlantUml.Queue(n + "_queue", None, None),
            PlantUml.Link(
              n + "_queue",
              n
            ),
            queueSkin
          )
            .pipe(PlantUml.ComponentDiagram.apply(_))

        case Title(s) =>
          PlantUml.ComponentDiagram:
            PlantUml.Title(List(s))

  given BrightEncoder[MermaidDiagram[Flowchart], DemoDsl] with
    def encodeBrightly(x: DemoDsl, isBright: Boolean): MermaidDiagram[Flowchart] =
      x match
        case ClusterService(n, asCluster) =>
          MermaidDiagram.empty

//          if asCluster then
//            Chain(1, 2, 3, 4)
//              .flatMap { i =>
//                Chain[PlantUml](
//                  PlantUml.Component(n + i.toString, None, Option.when(isBright)("Service")),
//                  skin(isBright)
//                )
//              }
//              .pipe(PlantUml.ComponentDiagram.apply(_))
//          else
//            Chain[PlantUml](
//              PlantUml.Component(n, None, Option.when(isBright)("Service")),
//              skin(isBright)
//            )
//              .pipe(PlantUml.ComponentDiagram.apply(_))

        case Buffered(n) =>
          MermaidDiagram.empty
//          Chain[PlantUml](
//            PlantUml.Component(n, None, Option.when(isBright)("Service")),
//            skin(isBright),
//            PlantUml.Queue(n + "_queue", None, None),
//            PlantUml.Link(
//              n + "_queue",
//              n
//            ),
//            queueSkin
//          )
//            .pipe(PlantUml.ComponentDiagram.apply(_))

        case Title(s) =>
          MermaidDiagram.empty

  private val queueSkin =
    PlantUml
      .SkinParamGroup("queue")
      .and("fontStyle", "bold")
      .and("fontColor", "#444")
      .and("backgroundColor", "#faf2c8/#e6c72c")
      .and("borderColor", "#807746")
      .and("borderThickness", "2")

  private def skin(isBright: Boolean) =
    if isBright then
      PlantUml
        .SkinParamGroup("component", "Service")
        .and("fontStyle", "bold")
        .and("fontColor", "white")
        .and("backgroundColor", "#7082b8/#283d7a")
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

  case class ConfigBasket(
      fooStyle: ConfigBasket.ServiceAppearance,
      barStyle: ConfigBasket.ServiceAppearance,
      title: String
  )

  object ConfigBasket:
    sealed trait ServiceAppearance

    object ServiceAppearance:
      case object AsSingleton extends ServiceAppearance

      case object AsCluster extends ServiceAppearance

      case object WithBuffer extends ServiceAppearance
