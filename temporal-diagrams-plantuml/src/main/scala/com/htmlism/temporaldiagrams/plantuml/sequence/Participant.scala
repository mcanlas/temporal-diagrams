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

  /**
    * A diagram participant with a shape, defined on a single line
    *
    * @param name
    *   The text that will display inside the shape; may be used to identify the participant in messages if an alias is
    *   not defined
    * @param shape
    *   The rendered shape of the participant
    * @param alias
    *   An optional value that will be used to identify the participant in messages, instead of the name value
    * @param order
    *   An optional integer that dictates the relative ordering of participants
    * @param color
    *   An optional value for the participant's background color
    */
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
