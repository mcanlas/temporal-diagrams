package com.htmlism.temporaldiagrams.plantuml.sequence

sealed trait Participant

object Participant:
  case class Basic(name: String)       extends Participant
  case class Actor(name: String)       extends Participant
  case class Boundary(name: String)    extends Participant
  case class Control(name: String)     extends Participant
  case class Entity(name: String)      extends Participant
  case class Database(name: String)    extends Participant
  case class Collections(name: String) extends Participant
  case class Queue(name: String)       extends Participant
