package com.htmlism.temporaldiagrams.demo

import scala.collection.immutable.ListSet
import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.HighlightEncoder
import com.htmlism.temporaldiagrams.Renderable
import com.htmlism.temporaldiagrams.Renderable.*
import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.syntax.*

// sbt "demo/runMain com.htmlism.temporaldiagrams.demo.WriteDemoDslDiagrams"
object WriteDemoDslDiagrams extends WriteDemoDslDiagrams[IO](FilePrinter[IO]) with IOApp.Simple

class WriteDemoDslDiagrams[F[_]: Applicative](out: FilePrinter[F]):
  def run: F[Unit] =
    runFor(PlantUml.render)

  private def runFor[D: Monoid](
      f: D => Chain[String]
  )(using HighlightEncoder[D, DemoDsl.Arrow], HighlightEncoder[D, DemoDsl]): F[Unit] =
    val toProducer =
      Kleisli.fromFunction[Id, DemoDsl.ConfigBasket.ServiceAppearance][Chain[
        Renderable.WithMultiArrows[D, String]
      ]]:
        case DemoDsl.ConfigBasket.ServiceAppearance.AsSingleton =>
          Chain(
            DemoDsl.ClusterService("foo", asCluster = false).tag("foo"),
            WithMultiArrows.Source("foo", List("foo"))
          )

        case DemoDsl.ConfigBasket.ServiceAppearance.AsCluster =>
          Chain(
            DemoDsl.ClusterService("foo", asCluster = true).tag("foo"),
            WithMultiArrows.Source("foo", (1 to 4).map("foo" + _).toList)
          )

        case DemoDsl.ConfigBasket.ServiceAppearance.WithBuffer =>
          Chain(
            DemoDsl.Buffered("foo").tag("foo"),
            WithMultiArrows.Source("foo", List("foo"))
          )

    val toConsumer =
      Kleisli.fromFunction[Id, DemoDsl.ConfigBasket.ServiceAppearance][Chain[
        Renderable.WithMultiArrows[D, String]
      ]]:
        case DemoDsl.ConfigBasket.ServiceAppearance.AsSingleton =>
          Chain(
            DemoDsl.ClusterService("bar", asCluster = false).tag("bar"),
            WithMultiArrows.MultiArrow("foo", "bar", ListSet.empty),
            WithMultiArrows.Destination("bar", List("bar"))
          )

        case DemoDsl.ConfigBasket.ServiceAppearance.AsCluster =>
          Chain(
            DemoDsl.ClusterService("bar", asCluster = true).tag("bar"),
            WithMultiArrows.MultiArrow("foo", "bar", ListSet.empty),
            WithMultiArrows.Destination("bar", (1 to 4).map("bar" + _).toList)
          )

        case DemoDsl.ConfigBasket.ServiceAppearance.WithBuffer =>
          Chain(
            DemoDsl.Buffered("bar").tag("bar"),
            WithMultiArrows.MultiArrow("foo", "bar", ListSet.empty),
            WithMultiArrows.Destination("bar", List("bar_queue"))
          )

    val toTitle =
      Kleisli.fromFunction[Id, String][Chain[Renderable.WithMultiArrows[D, String]]]: s =>
        Chain(
          DemoDsl.Title(s).r
        )

    val stackGivenCfg =
      (
        toTitle.local[DemoDsl.ConfigBasket](_.title) |+|
          toProducer.local[DemoDsl.ConfigBasket](_.fooStyle) |+|
          toConsumer.local[DemoDsl.ConfigBasket](_.barStyle)
      )
        .run
        .andThen(_.extract)

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

    val cfgs =
      initialDiagramConfig :: episodesDeltas

    def printNormalDiagram(renders: Chain[Renderable[D]], n: Int) =
      val str =
        renders
          .pipe(Renderable.renderMany[D])
          .pipe(f)
          .mkString_("\n")

      out.print(s"demo-dsl-$n.puml")(str)

    def printHighlightDiagrams(renders: Chain[Renderable[D]], n: Int) =
      val tags =
        Renderable
          .allTags(renders)
          .toList

      tags.traverse_ { t =>
        val str =
          renders
            .pipe(Renderable.renderManyWithTag[D](_, t))
            .pipe(f)
            .mkString_("\n")

        out.print(s"demo-dsl-$n-$t.puml")(str)
      }

    cfgs
      .zipWithIndex
      .traverse_ { case (cfg, n) =>
        val renders: Chain[Renderable[D]] =
          stackGivenCfg(cfg)
            .pipe: xs =>
              WithMultiArrows
                .renderArrows[DemoDsl.Arrow](xs)
                .getOrElse(sys.error("expected flawless arrow render"))

        printNormalDiagram(renders, n) *>
          printHighlightDiagrams(renders, n)
      }
