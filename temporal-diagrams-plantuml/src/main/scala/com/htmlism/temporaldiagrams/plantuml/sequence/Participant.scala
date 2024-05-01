package com.htmlism.temporaldiagrams.plantuml.sequence

case class Participant(
    id: String,
    shape: Participant.Shape = Participant.Shape.Default,
    name: Option[String]     = None,
    order: Option[Int]       = None,
    color: Option[String]    = None
)

object Participant:
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
