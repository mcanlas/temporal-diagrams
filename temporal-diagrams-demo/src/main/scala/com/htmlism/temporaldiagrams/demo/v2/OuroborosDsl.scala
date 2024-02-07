package com.htmlism.temporaldiagrams.demo.v2

import cats.data.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.v2.BrightEncoder

sealed trait OuroborosDsl

object OuroborosDsl:
  case class Config(variants: NonEmptyChain[Config.Variant], showMermaid: Boolean, showForeign: Boolean)

  object Config:
    case class Variant(namespace: String, encoder: Option[Encoder])

    enum Encoder:
      case ConfigToBusinessDsl
      case Highlight
      case Diagram

  case class Type(name: String, encoder: Option[String]) extends OuroborosDsl

  case class Output(name: String, encoder: Option[String]) extends OuroborosDsl

  case class Link(src: String, dest: String) extends OuroborosDsl

  given BrightEncoder[PlantUml.ComponentDiagram, OuroborosDsl] with
    def encodeBrightly(x: OuroborosDsl, isBright: Boolean): PlantUml.ComponentDiagram =
      x match
        case Type(name, oEncoder) =>
          PlantUml.ComponentDiagram(
            PlantUml
              .Component(name, safe(name).some, Option.when(isBright)("Type"))
              .applySome(oEncoder): (c, e) =>
                PlantUml.Package(e, c),
            if isBright then blue("component", "Type") else white("component")
          )

        case Output(name, oEncoder) =>
          PlantUml.ComponentDiagram(
            PlantUml
              .Database(name, safe(name).some, Option.when(isBright)("Text file"), Set.empty)
              .applySome(oEncoder): (c, e) =>
                PlantUml.Package(e, c),
            if isBright then red("database", "Text file") else white("database")
          )

        case Link(src, dest) =>
          PlantUml.ComponentDiagram:
            PlantUml
              .Link(safe(src), safe(dest), 2, PlantUml.Link.Direction.Forwards, PlantUml.Link.Weight.Solid, None)

  private def safe(s: String) =
    s
      .replaceAll("-", "_")
      .replaceAll(" ", "_")
      .replaceAll("\\(", "_")
      .replaceAll("\\)", "_")

  private def red(name: String, stereotype: String) =
    PlantUml
      .SkinParamGroup(name, stereotype)
      .and("fontStyle", "bold")
      .and("fontColor", "white")
      .and("backgroundColor", "#bc4f4f")
      .and("borderColor", "#642a2a")
      .and("borderThickness", "2")

  private def blue(name: String, stereotype: String) =
    PlantUml
      .SkinParamGroup(name, stereotype)
      .and("fontStyle", "bold")
      .and("fontColor", "white")
      .and("backgroundColor", "#586ba4")
      .and("borderColor", "#223336")
      .and("borderThickness", "2")

  private def white(name: String) =
    PlantUml
      .SkinParamGroup(name)
      .and("fontStyle", "bold")
      .and("fontColor", "#AAA")
      .and("backgroundColor", "white")
      .and("borderColor", "#AAA")
      .and("borderThickness", "2")
