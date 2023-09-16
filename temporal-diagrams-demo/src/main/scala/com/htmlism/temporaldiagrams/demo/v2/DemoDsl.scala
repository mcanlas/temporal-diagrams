package com.htmlism.temporaldiagrams.demo.v2

import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.v2.HighlightEncoder

sealed trait DemoDsl

object DemoDsl {
  case class Service(name: String, dependency: Option[String])  extends DemoDsl
  case class Hydra(name: String, dependency: Option[String])    extends DemoDsl
  case class Buffered(name: String, dependency: Option[String]) extends DemoDsl

  implicit val demoHighlightEncoder: HighlightEncoder[PlantUml, DemoDsl] =
    new HighlightEncoder[PlantUml, DemoDsl] {
      def encode(x: DemoDsl): PlantUml =
        PlantUml.Component("", None)

      def encodeWithHighlights(x: DemoDsl, highlighted: Boolean): PlantUml =
        PlantUml.Component("", None)
    }
}
