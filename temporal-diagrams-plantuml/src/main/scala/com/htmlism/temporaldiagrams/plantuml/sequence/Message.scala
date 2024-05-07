package com.htmlism.temporaldiagrams
package plantuml.sequence

import cats.data.Chain

case class Message(
    source: String,
    destination: String,
    direction: Message.Direction = Message.Direction.Forwards,
    style: Message.Style         = Message.Style.Solid,
    text: Option[String]         = None
):
  def withText(s: String): Message =
    copy(text = Some(s))

object Message:
  given DiagramEncoder[Message] with
    def encode(x: Message): Chain[String] =
      val Message(src, dest, dir, style, oText) = x

      val styleStr =
        style match
          case Style.Solid =>
            "-"

          case Style.Dotted =>
            "--"

      val arrow =
        dir match
          case Direction.Forwards =>
            styleStr + ">"

          case Direction.Backwards =>
            "<" + styleStr

      Chain.one:
        s"$src $arrow $dest"

  enum Direction:
    case Forwards, Backwards

  enum Style:
    case Solid, Dotted
