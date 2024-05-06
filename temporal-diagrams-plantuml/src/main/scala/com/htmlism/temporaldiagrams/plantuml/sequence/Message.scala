package com.htmlism.temporaldiagrams
package plantuml.sequence

import cats.data.Chain

case class Message(
    source: String,
    destination: String,
    direction: Message.Direction = Message.Direction.Forwards,
    style: Message.Style         = Message.Style.Solid,
    text: Option[String]         = None
)

object Message:
  given DiagramEncoder[Message] with
    def encode(x: Message): Chain[String] =
      val Message(src, dest, dir, style, oText) = x

      Chain.one:
        s"$src $dest"

  enum Direction:
    case Forwards, Backwards

  enum Style:
    case Solid, Dotted
