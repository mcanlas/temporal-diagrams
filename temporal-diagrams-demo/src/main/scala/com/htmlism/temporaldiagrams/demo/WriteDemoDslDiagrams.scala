package com.htmlism.temporaldiagrams.demo

import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.Renderable
import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.syntax.*

// sbt "demo/runMain com.htmlism.temporaldiagrams.demo.WriteDemoDslDiagrams"
object WriteDemoDslDiagrams extends WriteDemoDslDiagrams[IO](FilePrinter[IO]) with IOApp.Simple

class WriteDemoDslDiagrams[F[_]: Applicative](out: FilePrinter[F]):
  private val toProducer =
    Kleisli.fromFunction[Id, DemoDsl.ConfigBasket.ServiceAppearance][Renderable[PlantUml.ComponentDiagram]]:
      case DemoDsl.ConfigBasket.ServiceAppearance.AsSingleton =>
        DemoDsl.ClusterService("foo", None, asCluster = false).tag("foo")

      case DemoDsl.ConfigBasket.ServiceAppearance.AsCluster =>
        DemoDsl.ClusterService("foo", None, asCluster = true).tag("foo")

      case DemoDsl.ConfigBasket.ServiceAppearance.WithBuffer =>
        DemoDsl.Buffered("foo", None).tag("foo")

  private val toConsumer =
    Kleisli.fromFunction[Id, DemoDsl.ConfigBasket.ServiceAppearance][Renderable[PlantUml.ComponentDiagram]]:
      case DemoDsl.ConfigBasket.ServiceAppearance.AsSingleton =>
        DemoDsl.ClusterService("bar", "foo".some, asCluster = false).tag("bar")

      case DemoDsl.ConfigBasket.ServiceAppearance.AsCluster =>
        DemoDsl.ClusterService("bar", "foo".some, asCluster = true).tag("bar")

      case DemoDsl.ConfigBasket.ServiceAppearance.WithBuffer =>
        DemoDsl.Buffered("bar", "foo".some).tag("bar")

  private val toTitle =
    Kleisli.fromFunction[Id, String][Renderable[PlantUml.ComponentDiagram]]: s =>
      DemoDsl.Echo(PlantUml.Title(List(s))).r

  val stackGivenCfg =
    Chain(
      toTitle.local[DemoDsl.ConfigBasket](_.title),
      toProducer.local[DemoDsl.ConfigBasket](_.fooStyle),
      toConsumer.local[DemoDsl.ConfigBasket](_.barStyle)
    )
      .traverse(_.run)

  val initialDiagramConfig =
    DemoDsl.ConfigBasket(
      fooStyle = DemoDsl.ConfigBasket.ServiceAppearance.AsSingleton,
      DemoDsl.ConfigBasket.ServiceAppearance.AsSingleton,
      title = "Component diagram"
    )

  val episodesDeltas =
    List[DemoDsl.ConfigBasket => DemoDsl.ConfigBasket](
      _.copy(fooStyle = DemoDsl.ConfigBasket.ServiceAppearance.AsCluster),
      _.copy(barStyle = DemoDsl.ConfigBasket.ServiceAppearance.AsCluster),
      _.copy(barStyle = DemoDsl.ConfigBasket.ServiceAppearance.WithBuffer)
    )
      .mapAccumulate(initialDiagramConfig)((s, f) => f(s) -> f(s))
      ._2

  def run: F[Unit] =
    val cfgs =
      initialDiagramConfig :: episodesDeltas

    cfgs
      .zipWithIndex
      .traverse { case (cfg, n) =>
        val renders: Chain[Renderable[PlantUml.ComponentDiagram]] =
          stackGivenCfg(cfg)
            .map(_.extract)

        printNormalDiagram(renders, n) *> printHighlightDiagrams(renders, n)
      }
      .void

  private def printNormalDiagram(renders: Chain[Renderable[PlantUml.ComponentDiagram]], n: Int) =
    val str =
      renders
        .pipe(Renderable.renderMany[PlantUml.ComponentDiagram])
        .pipe(PlantUml.render)
        .mkString_("\n")

    out.print(s"demo-dsl-$n.puml")(str)

  private def printHighlightDiagrams(renders: Chain[Renderable[PlantUml.ComponentDiagram]], n: Int) =
    val tags =
      Renderable
        .allTags(renders)
        .toList

    tags.traverse_ { t =>
      val str =
        renders
          .pipe(Renderable.renderManyWithTag[PlantUml.ComponentDiagram](_, t))
          .pipe(PlantUml.render)
          .mkString_("\n")

      out.print(s"demo-dsl-$n-$t.puml")(str)
    }
