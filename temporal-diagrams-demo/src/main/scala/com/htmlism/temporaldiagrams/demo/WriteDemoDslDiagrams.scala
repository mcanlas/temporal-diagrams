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
import com.htmlism.temporaldiagrams.mermaid.*
import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.syntax.*

// sbt "demo/runMain com.htmlism.temporaldiagrams.demo.WriteDemoDslDiagrams"
object WriteDemoDslDiagrams extends WriteDemoDslDiagrams[IO](FilePrinter[IO]) with IOApp.Simple

class WriteDemoDslDiagrams[F[_]: Applicative: Parallel](out: FilePrinter[F]):
  def run: F[Unit] =
    List(
      runFor(MermaidDiagram.render[Flowchart], "mmd"),
      runFor(PlantUml.render, "puml")
    ).parSequence_

  private def runFor[D: Monoid](
      f: D => Chain[String],
      extension: String
  )(using HighlightEncoder[D, DemoDsl.Arrow], HighlightEncoder[D, DemoDsl]): F[Unit] =
    val toProducer =
      (_: DemoDsl.Config.ServiceAppearance) match
        case DemoDsl.Config.ServiceAppearance.AsSingleton =>
          Chain(
            DemoDsl.ClusterService("foo", asCluster = false).tag("foo"),
            WithMultiArrows.Source("foo", List("foo"))
          )

        case DemoDsl.Config.ServiceAppearance.AsCluster =>
          Chain(
            DemoDsl.ClusterService("foo", asCluster = true).tag("foo"),
            WithMultiArrows.Source("foo", (1 to 4).map("foo" + _).toList)
          )

        case DemoDsl.Config.ServiceAppearance.WithBuffer =>
          Chain(
            DemoDsl.Buffered("foo").tag("foo"),
            WithMultiArrows.Source("foo", List("foo"))
          )

    val toConsumer =
      (_: DemoDsl.Config.ServiceAppearance) match
        case DemoDsl.Config.ServiceAppearance.AsSingleton =>
          Chain(
            DemoDsl.ClusterService("bar", asCluster = false).tag("bar"),
            WithMultiArrows.MultiArrow("foo", "bar", ListSet.empty),
            WithMultiArrows.Destination("bar", List("bar"))
          )

        case DemoDsl.Config.ServiceAppearance.AsCluster =>
          Chain(
            DemoDsl.ClusterService("bar", asCluster = true).tag("bar"),
            WithMultiArrows.MultiArrow("foo", "bar", ListSet.empty),
            WithMultiArrows.Destination("bar", (1 to 4).map("bar" + _).toList)
          )

        case DemoDsl.Config.ServiceAppearance.WithBuffer =>
          Chain(
            DemoDsl.Buffered("bar").tag("bar"),
            WithMultiArrows.MultiArrow("foo", "bar", ListSet.empty),
            WithMultiArrows.Source("barqueue", List("bar_queue")),
            WithMultiArrows.Destination("barraw", List("bar")),
            WithMultiArrows.MultiArrow("barqueue", "barraw", ListSet.empty),
            WithMultiArrows.Destination("bar", List("bar_queue"))
          )

    val toLambda =
      (_: Unit) =>
        Chain(
          DemoDsl.Lambda("writer-lambda").tag("persistence")
        )

    val toService =
      (n: Int) =>
        if n == 1 then
          Chain(
            DemoDsl.Service("reader-service").r
          )
        else
          (1 to n)
            .map(m => DemoDsl.Service(s"reader-service-$m").r)
            .pipe(Chain.fromSeq)

    val toDatabase =
      (n: Int) =>
        Chain(
          DemoDsl.Database("database", n).tag("persistence")
        )

    val toTitle =
      (s: String) =>
        Chain(
          DemoDsl.Title(s).r
        )

    // use the LUB properties of list construction to coerce the non-multi-arrow and multi-arrow types together;
    // attempting to semigroup them individually will result in compilation errors
    val stackGivenCfg =
      NonEmptyList
        .of(
          toTitle.compose[DemoDsl.Config](_.title),
          toProducer.compose[DemoDsl.Config](_.fooStyle),
          toConsumer.compose[DemoDsl.Config](_.barStyle),
          toLambda.compose[DemoDsl.Config](_ => ()),
          toService.compose[DemoDsl.Config](_.serviceInstances),
          toDatabase.compose[DemoDsl.Config](_.databaseReplicas)
        )
        .reduce

    val initialDiagramConfig =
      DemoDsl.Config(
        fooStyle = DemoDsl.Config.ServiceAppearance.AsSingleton,
        DemoDsl.Config.ServiceAppearance.AsSingleton,
        title            = "Component diagram",
        databaseReplicas = 0,
        serviceInstances = 1
      )

    val episodesDeltas =
      List[DemoDsl.Config => DemoDsl.Config](
        _.copy(fooStyle = DemoDsl.Config.ServiceAppearance.AsCluster),
        _.copy(barStyle = DemoDsl.Config.ServiceAppearance.AsCluster),
        _.copy(barStyle = DemoDsl.Config.ServiceAppearance.WithBuffer),
        _.copy(databaseReplicas = 2),
        _.copy(serviceInstances = 3)
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

      out.print(s"demo-dsl-$n.$extension")(str)

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

        out.print(s"demo-dsl-$n-$t.$extension")(str)
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
