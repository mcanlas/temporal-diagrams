package com.htmlism.temporaldiagrams.demo.v2

import cats.data.Chain
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.plantuml.*
import com.htmlism.temporaldiagrams.v2.BrightEncoder

enum FraudEcosystemDsl:
  case EcsService(name: String)
  case Lambda(name: String)
  case Kinesis(name: String)
  case MySql(name: String)
  case DynamoDb(name: String)
  case Flink(name: String)
  case Link(src: String, comment: String, dest: String)

object FraudEcosystemDsl:
  enum PersistenceStyle:
    case MySql
    case DynamoDb

  enum QueueConsumerStyle:
    case EcsService
    case Lambda
    case Flink

  case class Config(
      happyPathStorage: PersistenceStyle,
      sadPathStorage: PersistenceStyle,
      queueConsumerStyle: QueueConsumerStyle
  )

  given BrightEncoder[Chain[PlantUml], FraudEcosystemDsl] with
    def encodeBrightly(x: FraudEcosystemDsl, isBright: Boolean): Chain[PlantUml] =
      x match
        case EcsService(name) =>
          Chain(
            PlantUml.Component(name, None, Option.when(isBright)("ECS Service")),
            if isBright then blue("component", "ECS Service") else white("component")
          )

        case Lambda(name) =>
          Chain(
            PlantUml.Component(name, None, Option.when(isBright)("Lambda")),
            if isBright then red("component", "Lambda") else white("component")
          )

        case Kinesis(name) =>
          Chain(
            PlantUml.Queue(name, None, Option.when(isBright)("Kinesis")),
            if isBright then red("queue", "Kinesis") else white("queue")
          )

        case MySql(name) =>
          Chain(
            PlantUml.Database(name, None, Option.when(isBright)("MySQL"), Nil),
            if isBright then blue("database", "MySQL") else white("database")
          )

        case DynamoDb(name) =>
          Chain(
            PlantUml.Database(name, None, Option.when(isBright)("DynamoDB"), Nil),
            if isBright then red("database", "DynamoDB") else white("database")
          )

        case Flink(name) =>
          Chain(
            PlantUml.Component(name, None, Option.when(isBright)("Flink")),
            if isBright then yellow("component", "Flink") else white("component")
          )

        case Link(src, comment, dest) =>
          Chain:
            PlantUml.Link(src, dest, 2, comment.some)

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

  private def yellow(name: String, stereotype: String) =
    PlantUml
      .SkinParamGroup(name, stereotype)
      .and("fontStyle", "bold")
      .and("fontColor", "#444")
      .and("backgroundColor", "#EEDD82")
      .and("borderColor", "#807746")
      .and("borderThickness", "2")

  private def white(name: String) =
    PlantUml
      .SkinParamGroup(name)
      .and("fontStyle", "bold")
      .and("fontColor", "#AAA")
      .and("backgroundColor", "white")
      .and("borderColor", "#AAA")
      .and("borderThickness", "2")
