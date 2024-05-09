package com.htmlism.temporaldiagrams
package plantuml.sequence

import cats.data.Chain

case class Participant(
    id: String,
    shape: Participant.Shape = Participant.Shape.Default,
    name: Option[String]     = None,
    order: Option[Int]       = None,
    color: Option[String]    = None
)

object Participant:
  given DiagramEncoder[Participant] with
    def encode(x: Participant): Chain[String] =
      val aliasStr =
        x
          .name
          .map(" as " + _)
          .fold("")

      Chain.one:
        s"${x.shape.s} ${x.id}$aliasStr"

  enum Shape(val s: String):
    case Default     extends Shape("participant")
    case Actor       extends Shape("actor")
    case Boundary    extends Shape("boundary")
    case Control     extends Shape("control")
    case Entity      extends Shape("entity")
    case Database    extends Shape("database")
    case Collections extends Shape("collections")
    case Queue       extends Shape("queue")

  case class MultiLine(id: String, xs: List[String], order: Option[Int] = None)
