package com.htmlism.temporaldiagrams.demo.v2

import cats.data.Chain
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.v2.BrightEncoder

sealed trait OuroborosDsl

object OuroborosDsl:
  case class Type(name: String) extends OuroborosDsl

  case class Encoding(src: String, dest: String, name: String) extends OuroborosDsl

  case class Output(language: String, namespace: String) extends OuroborosDsl

  given BrightEncoder[Chain[PlantUml], OuroborosDsl] with
    def encodeBrightly(x: OuroborosDsl, isBright: Boolean): Chain[PlantUml] =
      x match
        case Type(s) =>
          Chain(PlantUml.Component(s, None, None))

        case Encoding(src, dest, name) =>
          Chain(PlantUml.Arrow(src, dest, name.some))

        case Output(s, namespace) =>
          Chain(PlantUml.Database(s, Some(s + "_" + namespace), None, Nil))
