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
      ???

  enum Shape:
    case Default
    case Actor
    case Boundary
    case Control
    case Entity
    case Database
    case Collections
    case Queue

  case class MultiLine(id: String, xs: List[String], order: Option[Int] = None)
