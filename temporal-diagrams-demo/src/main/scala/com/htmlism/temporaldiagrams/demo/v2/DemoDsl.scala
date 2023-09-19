package com.htmlism.temporaldiagrams.demo.v2

import cats.data.NonEmptyList

import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.v2.HighlightEncoder

sealed trait DemoDsl

object DemoDsl {
  case class Service(name: String, dependency: Option[String])  extends DemoDsl
  case class Hydra(name: String, dependency: Option[String])    extends DemoDsl
  case class Buffered(name: String, dependency: Option[String]) extends DemoDsl

  implicit val demoHighlightEncoder: HighlightEncoder[NonEmptyList[PlantUml], DemoDsl] =
    new HighlightEncoder[NonEmptyList[PlantUml], DemoDsl] {
      def encode(x: DemoDsl): NonEmptyList[PlantUml] = {
        x match {
          case Service(n, _) =>
            NonEmptyList.one(PlantUml.Component(n, None))

          case Hydra(n, _) =>
            NonEmptyList.of(
              PlantUml.Component(n + 1.toString, None),
              PlantUml.Component(n + 2.toString, None),
              PlantUml.Component(n + 3.toString, None)
            )

          case Buffered(n, _) =>
            NonEmptyList.one(PlantUml.Component(n, None))
        }
      }

      def encodeWithHighlights(x: DemoDsl, highlighted: Boolean): NonEmptyList[PlantUml] =
        NonEmptyList.one(PlantUml.Component("", None))
    }
}
