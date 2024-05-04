package com.htmlism.temporaldiagrams
package plantuml.sequence

case class Message(
    source: String,
    destination: String,
    direction: Message.Direction = Message.Direction.Forwards,
    style: Message.Style         = Message.Style.Solid,
    text: Option[String]         = None
)

object Message:
  given DiagramEncoder[Message] = ???

  enum Direction:
    case Forwards, Backwards

  enum Style:
    case Solid, Dotted
