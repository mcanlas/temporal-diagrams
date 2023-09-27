package com.htmlism.temporaldiagrams.demo.v2

import cats.data.NonEmptyList

import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.v2.BrightEncoder

sealed trait OuroborosDsl

object OuroborosDsl {
  case class Type(name: String) extends OuroborosDsl

  case class Encoding(src: String, dest: String, name: String) extends OuroborosDsl

  case class Output(language: String) extends OuroborosDsl

  implicit val demoBrightEncoder: BrightEncoder[NonEmptyList[PlantUml], OuroborosDsl] =
    new BrightEncoder[NonEmptyList[PlantUml], OuroborosDsl] {
      def encodeBrightly(x: OuroborosDsl, isBright: Boolean): NonEmptyList[PlantUml] =
        x match {
          case Type(s) =>
            NonEmptyList
              .one(PlantUml.Component(s, None, None))

          case Encoding(src, dest, _) =>
            // TODO need arrow comment support
            NonEmptyList
              .one(PlantUml.Arrow(src, dest))

          case Output(s) =>
            NonEmptyList
              .one(PlantUml.Component(s, None, None))
        }
    }
}
