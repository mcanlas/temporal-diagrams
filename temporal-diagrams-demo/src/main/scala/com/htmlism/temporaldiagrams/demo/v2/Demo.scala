package com.htmlism.temporaldiagrams.demo.v2

import scala.util.chaining._

import cats._
import cats.data.Kleisli
import cats.data.NonEmptyList
import cats.effect._
import cats.syntax.all._

import com.htmlism.temporaldiagrams.demo.FilePrinterAlg
import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.v2.Renderable
import com.htmlism.temporaldiagrams.v2.syntax._

object Demo extends Demo[IO](FilePrinterAlg[IO]) with IOApp.Simple {
  sealed trait ServiceAppearance

  object ServiceAppearance {
    case object AsSingleton extends ServiceAppearance

    case object AsCluster extends ServiceAppearance

    case object WithBuffer extends ServiceAppearance
  }

  case class ConfigBasket(fooStyle: ServiceAppearance, barStyle: ServiceAppearance)
}

class Demo[F[_]: Applicative](out: FilePrinterAlg[F]) {
  private val toProducer =
    Kleisli.fromFunction[Id, Demo.ServiceAppearance][Renderable[NonEmptyList[PlantUml]]] {
      case Demo.ServiceAppearance.AsSingleton =>
        DemoDsl.ClusterService("foo", None, asCluster = false).tag("foo")

      case Demo.ServiceAppearance.AsCluster =>
        DemoDsl.ClusterService("foo", None, asCluster = true).tag("foo")

      case Demo.ServiceAppearance.WithBuffer =>
        DemoDsl.Buffered("foo", None).tag("foo")
    }

  private val toConsumer =
    Kleisli.fromFunction[Id, Demo.ServiceAppearance][Renderable[NonEmptyList[PlantUml]]] {
      case Demo.ServiceAppearance.AsSingleton =>
        DemoDsl.ClusterService("bar", "foo".some, asCluster = false).tag("bar")

      case Demo.ServiceAppearance.AsCluster =>
        DemoDsl.ClusterService("bar", "foo".some, asCluster = true).tag("bar")

      case Demo.ServiceAppearance.WithBuffer =>
        DemoDsl.Buffered("bar", "foo".some).tag("bar")
    }

  val stackGivenCfg =
    NonEmptyList
      .of(
        toProducer.local[Demo.ConfigBasket](_.fooStyle),
        toConsumer.local[Demo.ConfigBasket](_.barStyle)
      )
      .traverse(_.run)

  val z =
    Demo.ConfigBasket(fooStyle = Demo.ServiceAppearance.AsSingleton, Demo.ServiceAppearance.AsSingleton)

  val episodesDeltas =
    List[Demo.ConfigBasket => Demo.ConfigBasket](
      _.copy(fooStyle = Demo.ServiceAppearance.AsCluster),
      _.copy(barStyle = Demo.ServiceAppearance.AsCluster),
      _.copy(barStyle = Demo.ServiceAppearance.WithBuffer)
    )
      .mapAccumulate(z)((s, f) => f(s) -> f(s))
      ._2

  def run: F[Unit] = {
    val cfgs =
      z :: episodesDeltas

    cfgs
      .zipWithIndex
      .traverse { case (cfg, n) =>
        val renders: NonEmptyList[Renderable[NonEmptyList[PlantUml]]] =
          stackGivenCfg(cfg)
            .map(_.extract)

        printNormalDiagram(renders, n) *> printHighlightDiagrams(renders, n)
      }
      .void
  }

  private def printNormalDiagram(renders: NonEmptyList[Renderable[NonEmptyList[PlantUml]]], n: Int) = {
    val str =
      renders
        .pipe(Renderable.renderMany[NonEmptyList[PlantUml]])
        .pipe(_.distinct.sorted)
        .pipe(PlantUml.render(_))
        .mkString_("\n")

    out.print(s"v2-$n.puml")(str)
  }

  private def printHighlightDiagrams(renders: NonEmptyList[Renderable[NonEmptyList[PlantUml]]], n: Int) = {
    val tags =
      Renderable
        .allTags(renders)
        .toList

    tags.traverse_ { t =>
      val str =
        renders
          .pipe(Renderable.renderManyWithTag[NonEmptyList[PlantUml]](_, t))
          .pipe(_.distinct.sorted)
          .pipe(PlantUml.render(_))
          .mkString_("\n")

      out.print(s"v2-$n-$t.puml")(str)
    }
  }
}
