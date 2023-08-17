package com.htmlism.temporaldiagrams.plantuml

import cats.data.NonEmptyList

import com.htmlism.temporaldiagrams.v2.Encoder

case class Component(name: String, alias: Option[String])

object Component {
  implicit val componentEncoder: Encoder[PlantUml, Component] =
    new Encoder[PlantUml, Component] {
      def encode(x: Component): NonEmptyList[String] =
        NonEmptyList.of("")

      def encodeWithHighlights(x: Component, highlighted: Boolean): NonEmptyList[String] =
        NonEmptyList.of("")
    }
}
