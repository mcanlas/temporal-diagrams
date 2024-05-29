package com.htmlism.temporaldiagrams
package plantuml.sequence

sealed trait Participant

object Participant:
  enum Shape(val s: String):
    case Default     extends Shape("participant")
    case Actor       extends Shape("actor")
    case Boundary    extends Shape("boundary")
    case Control     extends Shape("control")
    case Entity      extends Shape("entity")
    case Database    extends Shape("database")
    case Collections extends Shape("collections")
    case Queue       extends Shape("queue")

  case class Basic(
      name: String,
      shape: Participant.Shape = Participant.Shape.Default,
      alias: Option[String]    = None,
      order: Option[Int]       = None,
      color: Option[String]    = None
  ) extends Participant:
    def withAlias(s: String): Participant.Basic =
      copy(alias = Some(s))

    def withOrder(n: Int): Participant.Basic =
      copy(order = Some(n))

    def withColor(s: String): Participant.Basic =
      copy(color = Some(s))

  case class MultiLine(id: String, xs: List[String], order: Option[Int] = None) extends Participant
