package com.htmlism.temporaldiagrams.plantuml

import cats.data.NonEmptyList

import com.htmlism.temporaldiagrams.v2._

/**
  * A building block in component diagrams
  */
case class Component(name: String, alias: Option[String])

object Component {
  implicit val componentEncoder: BrightEncoder[PlantUml, Component] =
    new BrightEncoder[PlantUml, Component] {
      def bright(x: Component): NonEmptyList[String] =
        NonEmptyList.of("")

      def dim(x: Component): NonEmptyList[String] =
        NonEmptyList.of("")
    }
}
