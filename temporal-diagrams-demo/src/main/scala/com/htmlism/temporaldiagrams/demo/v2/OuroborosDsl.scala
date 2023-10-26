package com.htmlism.temporaldiagrams.demo.v2

import cats.data.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.v2.BrightEncoder

sealed trait OuroborosDsl

object OuroborosDsl:
  case class Config(variants: NonEmptyChain[Config.Variant], showMermaid: Boolean)

  object Config:
    case class Variant(namespace: String, encoder: Option[Encoder])

    enum Encoder:
      case ConfigToBusinessDsl
      case Highlight
      case Diagram

  case class Type(name: String, encoder: Option[String]) extends OuroborosDsl

  case class Output(name: String, encoder: Option[String]) extends OuroborosDsl

  case class Link(src: String, dest: String) extends OuroborosDsl

  given BrightEncoder[Chain[PlantUml], OuroborosDsl] with
    def encodeBrightly(x: OuroborosDsl, isBright: Boolean): Chain[PlantUml] =
      x match
        case Type(name, oEncoder) =>
          Chain(
            PlantUml
              .Component(name, safe(name).some, None)
              .applySome(oEncoder): (c, e) =>
                PlantUml.Package(e, c)
          )

        case Output(name, oEncoder) =>
          Chain(
            PlantUml
              .Database(name, safe(name).some, None, Nil)
              .applySome(oEncoder): (c, e) =>
                PlantUml.Package(e, c)
          )

        case Link(src, dest) =>
          Chain:
            PlantUml
              .Arrow(safe(src), safe(dest), None)

  private def safe(s: String) =
    s
      .replaceAll("-", "_")
      .replaceAll(" ", "_")
      .replaceAll("\\(", "_")
      .replaceAll("\\)", "_")
