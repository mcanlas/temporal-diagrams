package com.htmlism.temporaldiagrams.plantuml

import cats.data.NonEmptyList

import com.htmlism.temporaldiagrams.v2._

/**
  * A directed line from the source to the destination
  *
  * In terms of "gravity", the source is always first. In top-down diagrams, the source is on the top and the
  * destination is on the bottom. In left-to-right diagrams, the source is on the left and the destination is on the
  * right.
  */
case class Arrow(source: String, destination: String)

object Arrow {
  implicit val arrowEncoder: Encoder[PlantUml, Arrow] =
    new Encoder[PlantUml, Arrow] {
      def encode(x: Arrow): NonEmptyList[String] =
        NonEmptyList.one("")
    }
}
