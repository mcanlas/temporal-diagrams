package com.htmlism.temporaldiagrams.plantuml

import cats.data.NonEmptyList

import com.htmlism.temporaldiagrams.v2.Encoder

case class Arrow(source: String, destination: String)

object Arrow {
  implicit val arrowEncoder: Encoder[PlantUml, Arrow] =
    new Encoder[PlantUml, Arrow] {
      def encode(x: Arrow): NonEmptyList[String] =
        NonEmptyList.of("")

      def encodeWithHighlights(x: Arrow, highlighted: Boolean): NonEmptyList[String] =
        NonEmptyList.of("")
    }
}
