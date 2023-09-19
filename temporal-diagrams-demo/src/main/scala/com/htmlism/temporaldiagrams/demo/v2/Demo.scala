package com.htmlism.temporaldiagrams.demo.v2

import cats._
import cats.data.Kleisli
import cats.data.NonEmptyList
import cats.effect._
import cats.effect.std.Console
import cats.syntax.all._

import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.v2.Renderable
import com.htmlism.temporaldiagrams.v2.syntax._

object Demo extends Demo[IO] with IOApp.Simple {
  sealed trait BarAppearance

  object BarAppearance {
    case object AsService extends BarAppearance

    case object AsHydra extends BarAppearance

    case object WithBuffer extends BarAppearance
  }

  case class ConfigBasket(isNew: Boolean, barStyle: BarAppearance)
}

class Demo[F[_]: Applicative](implicit out: Console[F]) {
  private val toProducer =
    Kleisli.fromFunction[Id, Boolean][Renderable[NonEmptyList[PlantUml]]] { asNew =>
      if (asNew)
        DemoDsl.Service("new_foo", None)
      else
        DemoDsl.Service("foo", None).tag("foo")
    }

  private val toConsumer =
    Kleisli.fromFunction[Id, Demo.BarAppearance][Renderable[NonEmptyList[PlantUml]]] {
      case Demo.BarAppearance.AsService =>
        DemoDsl.Service("bar", None).tag("bar")

      case Demo.BarAppearance.AsHydra =>
        DemoDsl.Hydra("bar", None).tag("bar")

      case Demo.BarAppearance.WithBuffer =>
        DemoDsl.Buffered("bar", None)
    }

  val renderBig =
    NonEmptyList
      .of(
        toProducer.local[Demo.ConfigBasket](_.isNew),
        toConsumer.local[Demo.ConfigBasket](_.barStyle)
      )
      .traverse(_.run)
      .andThen(xs => xs.map(x => x: Renderable[NonEmptyList[PlantUml]]))
      .andThen(xs => Renderable.renderMany[NonEmptyList[PlantUml]](xs))
      .andThen(PlantUml.render[NonEmptyList[PlantUml]])

  val episodes =
    NonEmptyList.of(
      Demo.ConfigBasket(isNew = false, Demo.BarAppearance.AsService),
      Demo.ConfigBasket(isNew = true, Demo.BarAppearance.AsService),
      Demo.ConfigBasket(isNew = true, Demo.BarAppearance.AsHydra),
      Demo.ConfigBasket(isNew = true, Demo.BarAppearance.WithBuffer)
    )

  def run: F[Unit] = {
    episodes
      .traverse(e => out.println(renderBig(e)))
      .void
  }
}
