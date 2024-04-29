package com.htmlism.temporaldiagrams.plantuml.sequence

case class Message()

object Message:
  enum Direction:
    case Forwards, Backwards

  enum Style:
    case Solid, Dotted
