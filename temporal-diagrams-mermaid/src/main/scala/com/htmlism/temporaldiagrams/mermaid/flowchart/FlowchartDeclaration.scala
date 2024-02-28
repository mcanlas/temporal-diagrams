package com.htmlism.temporaldiagrams.mermaid.flowchart

import cats.data.Chain

sealed trait FlowchartDeclaration

object FlowchartDeclaration:
  sealed trait Node extends FlowchartDeclaration

  sealed trait Link extends FlowchartDeclaration

  private def encodeNode(left: String, right: String)(id: String, text: String) =
    Chain.one:
      s"$id$left$text$right"

  def encode(x: FlowchartDeclaration): Chain[String] =
    x match
      case Node.Square(id, oText) =>
        Chain.one:
          id + oText.map(s => s"[$s]").getOrElse("")

      case Node.Round(id, text) =>
        encodeNode("(", ")")(id, text)

      case Node.Stadium(id, text) =>
        encodeNode("([", "])")(id, text)

      case Node.Subroutine(id, text) =>
        encodeNode("[[", "]]")(id, text)

      case Node.Cylinder(id, text) =>
        encodeNode("[(", ")]")(id, text)

      case Node.Circle(id, text) =>
        encodeNode("((", "))")(id, text)

      case Node.Asymmetric(id, text) =>
        encodeNode(">", "]")(id, text)

      case Node.Rhombus(id, text) =>
        encodeNode("{", "}")(id, text)

      case Node.Hexagon(id, text) =>
        encodeNode("{{", "}}")(id, text)

      case Node.Parallelogram(id, text) =>
        encodeNode("[/", "/]")(id, text)

      case Node.ParallelogramAlt(id, text) =>
        encodeNode("[\\", "\\]")(id, text)

      case Node.Trapezoid(id, text) =>
        encodeNode("[/", "\\]")(id, text)

      case Node.TrapezoidAlt(id, text) =>
        encodeNode("[\\", "/]")(id, text)

      case Node.DoubleCircle(id, text) =>
        encodeNode("(((", ")))")(id, text)

  object Node:
    case class Square(id: String, text: Option[String]) extends Node:
      def withText(s: String): Node =
        copy(text = Some(s))

    case class Round(id: String, text: String) extends Node

    case class Stadium(id: String, text: String) extends Node

    case class Subroutine(id: String, text: String) extends Node

    case class Cylinder(id: String, text: String) extends Node

    case class Circle(id: String, text: String) extends Node

    case class Asymmetric(id: String, text: String) extends Node

    case class Rhombus(id: String, text: String) extends Node

    case class Hexagon(id: String, text: String) extends Node

    case class Parallelogram(id: String, text: String) extends Node

    case class ParallelogramAlt(id: String, text: String) extends Node

    case class Trapezoid(id: String, text: String) extends Node

    case class TrapezoidAlt(id: String, text: String) extends Node

    case class DoubleCircle(id: String, text: String) extends Node
