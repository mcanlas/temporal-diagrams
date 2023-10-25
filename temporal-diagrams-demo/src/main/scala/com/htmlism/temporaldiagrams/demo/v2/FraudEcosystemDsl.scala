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
          Chain:
            PlantUml.Component(name, None, Option.when(isBright)("ECS Service"))

        case Lambda(name) =>
          Chain:
            PlantUml.Component(name, None, Option.when(isBright)("Lambda"))

        case Kinesis(name) =>
          Chain:
            PlantUml.Queue(name, None, Option.when(isBright)("Kinesis"))

        case MySql(name) =>
          Chain:
            PlantUml.Database(name, None, Option.when(isBright)("MySQL"), Nil)

        case DynamoDb(name) =>
          Chain:
            PlantUml.Database(name, None, Option.when(isBright)("DynamoDB"), Nil)

        case Flink(name) =>
          Chain:
            PlantUml.Component(name, None, Option.when(isBright)("Flink"))

        case Link(src, comment, dest) =>
          Chain:
            PlantUml.Arrow(src, dest, comment.some)
