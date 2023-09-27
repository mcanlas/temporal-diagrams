package com.htmlism.temporaldiagrams.demo.v2

import cats.data.NonEmptyChain

import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.v2.BrightEncoder

sealed trait OuroborosDsl

object OuroborosDsl {
  case class Type(name: String) extends OuroborosDsl

  case class Encoding(src: String, dest: String, name: String) extends OuroborosDsl

  case class Output(language: String) extends OuroborosDsl

  implicit val demoBrightEncoder: BrightEncoder[NonEmptyChain[PlantUml], OuroborosDsl] =
    new BrightEncoder[NonEmptyChain[PlantUml], OuroborosDsl] {
      def encodeBrightly(x: OuroborosDsl, isBright: Boolean): NonEmptyChain[PlantUml] =
        x match {
          case Type(s) =>
            NonEmptyChain
              .one(PlantUml.Component(s, None, None))

          case Encoding(src, dest, _) =>
            // TODO need arrow comment support
            NonEmptyChain
              .one(PlantUml.Arrow(src, dest))

          case Output(s) =>
            NonEmptyChain
              .one(PlantUml.Component(s, None, None))
        }
    }
}
