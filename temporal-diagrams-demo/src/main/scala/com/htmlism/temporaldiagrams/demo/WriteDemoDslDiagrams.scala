package com.htmlism.temporaldiagrams.demo

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
    val toLambda =
      (_: Unit) =>
        Chain(
          DemoDsl.Lambda("writer-lambda").tag("persistence")
        )

    val toService =
      (n: Int) =>
        if n == 1 then
          Chain(
            DemoDsl.Service("reader-service").r,
            WithMultiArrows.Source("readers", List("reader-service")),
            WithMultiArrows
              .MultiArrow("readers", "database-read", (src, dest) => DemoDsl.Arrow(src, dest, "reads from".some))
          )
        else
          val services =
            (1 to n)
              .map(m => DemoDsl.Service(s"reader-service-$m").r)
              .toList

          val srcs =
            (1 to n)
              .map(m => s"reader-service-$m")
              .toList
              .pipe(WithMultiArrows.Source("readers", _))

          Chain.fromSeq(
            WithMultiArrows.MultiArrow(
              "readers",
              "database-read",
              (src, dest) => DemoDsl.Arrow(src, dest)
            ) :: srcs :: services
          )

    val toDatabase =
      (n: Int) =>
        if n == 0 then
          Chain(
            DemoDsl.Database("database", n).tag("persistence"),
            WithMultiArrows.Destination("database-read", List("database"))
          )
        else
          val destinations =
            (1 to n)
              .map(m => s"replica-$m")
              .toList

          Chain(
            DemoDsl.Database("database", n).tag("persistence"),
            WithMultiArrows.Destination("database-read", destinations)
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
          toLambda.compose[DemoDsl.Config](_ => ()),
          toService.compose[DemoDsl.Config](_.serviceInstances),
          toDatabase.compose[DemoDsl.Config](_.databaseReplicas)
        )
        .reduce

    val initialDiagramConfig =
      DemoDsl.Config(
        title            = "Component diagram",
        databaseReplicas = 0,
        serviceInstances = 1
      )

    val episodesDeltas =
      List[DemoDsl.Config => DemoDsl.Config](
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
                .renderArrows(xs)
                .getOrElse(sys.error("expected flawless arrow render"))

        printNormalDiagram(renders, n) *>
          printHighlightDiagrams(renders, n)
      }
