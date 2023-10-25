package com.htmlism.temporaldiagrams.demo.v2

import scala.util.chaining.*

import cats.*
import cats.data.Chain
import cats.data.Kleisli
import cats.effect.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.demo.FilePrinter
import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.v2.Renderable
import com.htmlism.temporaldiagrams.v2.syntax.*

object WriteFraudEcosystemDiagrams extends WriteFraudEcosystemDiagrams[IO](FilePrinter[IO]) with IOApp.Simple

class WriteFraudEcosystemDiagrams[F[_]: Applicative](out: FilePrinter[F]):
  def storage(name: String) =
    Kleisli.fromFunction[Id, FraudEcosystemDsl.PersistenceStyle][Renderable[Chain[PlantUml]]]:
      case FraudEcosystemDsl.PersistenceStyle.DynamoDb =>
        FraudEcosystemDsl.DynamoDb(name)

      case FraudEcosystemDsl.PersistenceStyle.MySql =>
        FraudEcosystemDsl.MySql(name)

  def queueConsumer(name: String) =
    Kleisli.fromFunction[Id, FraudEcosystemDsl.QueueConsumerStyle][Renderable[Chain[PlantUml]]]:
      case FraudEcosystemDsl.QueueConsumerStyle.Lambda =>
        FraudEcosystemDsl.Lambda(name)

      case FraudEcosystemDsl.QueueConsumerStyle.EcsService =>
        FraudEcosystemDsl.EcsService(name)

      case FraudEcosystemDsl.QueueConsumerStyle.Flink =>
        FraudEcosystemDsl.Flink(name)

  val pure =
    Chain[Renderable[Chain[PlantUml]]](
      FraudEcosystemDsl.EcsService("edge"),
      FraudEcosystemDsl.EcsService("fraud_service"),
      FraudEcosystemDsl.Link("edge", "forwards requests to", "fraud_service"),
      FraudEcosystemDsl.Link("fraud_service", "writes to", "user_activity"),
      FraudEcosystemDsl.Link("fraud_service", "writes to", "fraud_history")
    )
      .map(Kleisli.pure[Id, FraudEcosystemDsl.Config, Renderable[Chain[PlantUml]]])

  val stackGivenCfg =
    Chain(
      storage("user_activity").local[FraudEcosystemDsl.Config](_.happyPathStorage),
      storage("fraud_history").local[FraudEcosystemDsl.Config](_.sadPathStorage),
      queueConsumer("fraud_consumer").local[FraudEcosystemDsl.Config](_.queueConsumerStyle)
    )
      .concat(pure)
      .traverse(_.run)

  val z =
    FraudEcosystemDsl.Config(
      FraudEcosystemDsl.PersistenceStyle.MySql,
      FraudEcosystemDsl.PersistenceStyle.MySql,
      FraudEcosystemDsl.QueueConsumerStyle.Lambda
    )

  val episodesDeltas =
    List[FraudEcosystemDsl.Config => FraudEcosystemDsl.Config](
      _.copy(sadPathStorage = FraudEcosystemDsl.PersistenceStyle.DynamoDb),
      _.copy(queueConsumerStyle = FraudEcosystemDsl.QueueConsumerStyle.EcsService),
      _.copy(queueConsumerStyle = FraudEcosystemDsl.QueueConsumerStyle.Flink)
    )
      .mapAccumulate(z)((s, f) => f(s) -> f(s))
      ._2

  def run: F[Unit] =
    val cfgs =
      z :: episodesDeltas

    cfgs
      .zipWithIndex
      .traverse { case (cfg, n) =>
        val renders: Chain[Renderable[Chain[PlantUml]]] =
          stackGivenCfg(cfg)
            .map(_.extract)

        printNormalDiagram(renders, n) *> printHighlightDiagrams(renders, n)
      }
      .void

  private def printNormalDiagram(renders: Chain[Renderable[Chain[PlantUml]]], n: Int) =
    val str =
      renders
        .pipe(Renderable.renderMany[Chain[PlantUml]])
        .pipe(PlantUml.renderHorizontally)
        .mkString_("\n")

    out.print(s"fraud-$n.puml")(str)

  private def printHighlightDiagrams(renders: Chain[Renderable[Chain[PlantUml]]], n: Int) =
    val tags =
      Renderable
        .allTags(renders)
        .toList

    tags.traverse_ { t =>
      val str =
        renders
          .pipe(Renderable.renderManyWithTag[Chain[PlantUml]](_, t))
          .pipe(PlantUml.renderHorizontally)
          .mkString_("\n")

      out.print(s"fraud-$n-$t.puml")(str)
    }
