package com.htmlism.temporaldiagrams.demo

import com.htmlism.temporaldiagrams.BrightEncoder
import com.htmlism.temporaldiagrams.plantuml.*

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

  given BrightEncoder[PlantUml.ComponentDiagram, FraudEcosystemDsl] with
    def encodeBrightly(x: FraudEcosystemDsl, isBright: Boolean): PlantUml.ComponentDiagram =
      x match
        case EcsService(name) =>
          PlantUml.ComponentDiagram(
            PlantUml.Component(name, None, Option.when(isBright)("ECS Service")),
            if isBright then blue("component", "ECS Service") else white("component")
          )

        case Lambda(name) =>
          PlantUml.ComponentDiagram(
            PlantUml.Component(name, None, Option.when(isBright)("Lambda")),
            if isBright then red("component", "Lambda") else white("component")
          )

        case Kinesis(name) =>
          PlantUml.ComponentDiagram(
            PlantUml.Queue(name, None, Option.when(isBright)("Kinesis")),
            if isBright then red("queue", "Kinesis") else white("queue")
          )

        case MySql(name) =>
          PlantUml.ComponentDiagram(
            PlantUml.Database(name, None, Option.when(isBright)("MySQL"), Set.empty),
            if isBright then blue("database", "MySQL") else white("database")
          )

        case DynamoDb(name) =>
          PlantUml.ComponentDiagram(
            PlantUml.Database(name, None, Option.when(isBright)("DynamoDB"), Set.empty),
            if isBright then red("database", "DynamoDB") else white("database")
          )

        case Flink(name) =>
          PlantUml.ComponentDiagram(
            PlantUml.Component(name, None, Option.when(isBright)("Flink")),
            if isBright then yellow("component", "Flink") else white("component")
          )

        case Link(src, comment, dest) =>
          PlantUml.ComponentDiagram:
            PlantUml
              .Link(
                src,
                dest
              )
              .withText(comment)

  private def red(name: String, stereotype: String) =
    PlantUml
      .SkinParamGroup(name, stereotype)
      .and("fontStyle", "bold")
      .and("fontColor", "white")
      .and("backgroundColor", "#d47777/#822323")
      .and("borderColor", "#642a2a")
      .and("borderThickness", "2")

  private def blue(name: String, stereotype: String) =
    PlantUml
      .SkinParamGroup(name, stereotype)
      .and("fontStyle", "bold")
      .and("fontColor", "white")
      .and("backgroundColor", "#7082b8/#283d7a")
      .and("borderColor", "#223336")
      .and("borderThickness", "2")

  private def yellow(name: String, stereotype: String) =
    PlantUml
      .SkinParamGroup(name, stereotype)
      .and("fontStyle", "bold")
      .and("fontColor", "#444")
      .and("backgroundColor", "#faf2c8/#e6c72c")
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
