package com.htmlism.temporaldiagrams.demo.v2

import cats.data.NonEmptyList
import cats.syntax.all._

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
            NonEmptyList.of(
              PlantUml.Component(n, None, "Service".some),
              PlantUml
                .SkinParamGroup("component", "Service")
                .and("fontStyle", "bold")
                .and("fontColor", "white")
                .and("backgroundColor", "#586ba4")
                .and("borderColor", "#223336")
                .and("borderThickness", "2")
            )

          case Hydra(n, _) =>
            NonEmptyList.of(
              PlantUml.Component(n + 1.toString, None, "Service".some),
              PlantUml.Component(n + 2.toString, None, "Service".some),
              PlantUml.Component(n + 3.toString, None, "Service".some)
            )

          case Buffered(n, _) =>
            NonEmptyList.one(PlantUml.Component(n, None, "Service".some))
        }
      }

      def encodeWithHighlights(x: DemoDsl, highlighted: Boolean): NonEmptyList[PlantUml] =
        NonEmptyList.one(PlantUml.Component("", None, "Service".some))
    }
}
