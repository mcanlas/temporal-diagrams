package com.htmlism.temporaldiagrams.demo.v2

import scala.util.chaining._

import cats._
import cats.data.Kleisli
import cats.data.NonEmptyChain
import cats.effect._
import cats.syntax.all._

import com.htmlism.temporaldiagrams.demo.FilePrinterAlg
import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.v2.Renderable
import com.htmlism.temporaldiagrams.v2.syntax._

object WriteDemoDslDiagrams extends WriteDemoDslDiagrams[IO](FilePrinterAlg[IO]) with IOApp.Simple

class WriteDemoDslDiagrams[F[_]: Applicative](out: FilePrinterAlg[F]) {
  private val toProducer =
    Kleisli.fromFunction[Id, DemoDsl.ConfigBasket.ServiceAppearance][Renderable[NonEmptyChain[PlantUml]]] {
      case DemoDsl.ConfigBasket.ServiceAppearance.AsSingleton =>
        DemoDsl.ClusterService("foo", None, asCluster = false).tag("foo")

      case DemoDsl.ConfigBasket.ServiceAppearance.AsCluster =>
        DemoDsl.ClusterService("foo", None, asCluster = true).tag("foo")

      case DemoDsl.ConfigBasket.ServiceAppearance.WithBuffer =>
        DemoDsl.Buffered("foo", None).tag("foo")
    }

  private val toConsumer =
    Kleisli.fromFunction[Id, DemoDsl.ConfigBasket.ServiceAppearance][Renderable[NonEmptyChain[PlantUml]]] {
      case DemoDsl.ConfigBasket.ServiceAppearance.AsSingleton =>
        DemoDsl.ClusterService("bar", "foo".some, asCluster = false).tag("bar")

      case DemoDsl.ConfigBasket.ServiceAppearance.AsCluster =>
        DemoDsl.ClusterService("bar", "foo".some, asCluster = true).tag("bar")

      case DemoDsl.ConfigBasket.ServiceAppearance.WithBuffer =>
        DemoDsl.Buffered("bar", "foo".some).tag("bar")
    }

  val stackGivenCfg =
    NonEmptyChain
      .of(
        toProducer.local[DemoDsl.ConfigBasket](_.fooStyle),
        toConsumer.local[DemoDsl.ConfigBasket](_.barStyle)
      )
      .traverse(_.run)

  val z =
    DemoDsl.ConfigBasket(
      fooStyle = DemoDsl.ConfigBasket.ServiceAppearance.AsSingleton,
      DemoDsl.ConfigBasket.ServiceAppearance.AsSingleton
    )

  val episodesDeltas =
    List[DemoDsl.ConfigBasket => DemoDsl.ConfigBasket](
      _.copy(fooStyle = DemoDsl.ConfigBasket.ServiceAppearance.AsCluster),
      _.copy(barStyle = DemoDsl.ConfigBasket.ServiceAppearance.AsCluster),
      _.copy(barStyle = DemoDsl.ConfigBasket.ServiceAppearance.WithBuffer)
    )
      .mapAccumulate(z)((s, f) => f(s) -> f(s))
      ._2

  def run: F[Unit] = {
    val cfgs =
      z :: episodesDeltas

    cfgs
      .zipWithIndex
      .traverse { case (cfg, n) =>
        val renders: NonEmptyChain[Renderable[NonEmptyChain[PlantUml]]] =
          stackGivenCfg(cfg)
            .map(_.extract)

        printNormalDiagram(renders, n) *> printHighlightDiagrams(renders, n)
      }
      .void
  }

  private def printNormalDiagram(renders: NonEmptyChain[Renderable[NonEmptyChain[PlantUml]]], n: Int) = {
    val str =
      renders
        .pipe(Renderable.renderMany[NonEmptyChain[PlantUml]])
        .pipe(PlantUml.render)
        .mkString_("\n")

    out.print(s"v2-$n.puml")(str)
  }

  private def printHighlightDiagrams(renders: NonEmptyChain[Renderable[NonEmptyChain[PlantUml]]], n: Int) = {
    val tags =
      Renderable
        .allTags(renders)
        .toList

    tags.traverse_ { t =>
      val str =
        renders
          .pipe(Renderable.renderManyWithTag[NonEmptyChain[PlantUml]](_, t))
          .pipe(PlantUml.render)
          .mkString_("\n")

      out.print(s"v2-$n-$t.puml")(str)
    }
  }
}
