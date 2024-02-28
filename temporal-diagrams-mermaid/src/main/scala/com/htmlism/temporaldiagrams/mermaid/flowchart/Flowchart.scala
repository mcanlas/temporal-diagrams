package com.htmlism.temporaldiagrams.mermaid
package flowchart

import cats.data.Chain

sealed trait Flowchart

object Flowchart:
  type LR = LeftToRight
  type RL = RightToLeft
  type TD = TopDown
  type TB = TopToBottom
  type BT = BottomToTop

  private def encodeNode(left: String, right: String)(id: String, text: String) =
    Chain.one:
      s"$id$left$text$right"

  case class CommonEncoder[A <: Flowchart](s: String) extends MermaidDiagramEncoder[A]:
    def header: String =
      s

    def encode(x: A): Chain[String] =
      x match
        case Node(id, oText) =>
          Chain.one:
            id + oText.map(s => s"[$s]").getOrElse("")

        case Node.Round(id, text) =>
          encodeNode("(", ")")(id, text)

        case Node.Stadium(id, text) =>
          encodeNode("(", ")")(id, text)

        case Node.Subroutine(id, text) =>
          encodeNode("(", ")")(id, text)

        case Node.Cylinder(id, text) =>
          encodeNode("(", ")")(id, text)

        case Node.Circle(id, text) =>
          encodeNode("(", ")")(id, text)

        case Node.Asymmetric(id, text) =>
          encodeNode("(", ")")(id, text)

        case Node.Rhombus(id, text) =>
          encodeNode("(", ")")(id, text)

        case Node.Hexagon(id, text) =>
          encodeNode("(", ")")(id, text)

        case Node.Parallelogram(id, text) =>
          encodeNode("(", ")")(id, text)

        case Node.ParallelogramAlt(id, text) =>
          encodeNode("(", ")")(id, text)

        case Node.Trapezoid(id, text) =>
          encodeNode("(", ")")(id, text)

        case Node.TrapezoidAlt(id, text) =>
          encodeNode("(", ")")(id, text)

        case Node.DoubleCircle(id, text) =>
          encodeNode("(", ")")(id, text)

  given MermaidDiagramEncoder[Flowchart] =
    CommonEncoder("flowchart")

  sealed trait LeftToRight extends Flowchart

  object LeftToRight:
    given MermaidDiagramEncoder[LeftToRight] =
      CommonEncoder("flowchart LR")

  sealed trait RightToLeft extends Flowchart

  object RightToLeft:
    given MermaidDiagramEncoder[RightToLeft] =
      CommonEncoder("flowchart RL")

  sealed trait TopDown extends Flowchart

  object TopDown:
    given MermaidDiagramEncoder[TopDown] =
      CommonEncoder("flowchart TD")

  sealed trait TopToBottom extends Flowchart

  object TopToBottom:
    given MermaidDiagramEncoder[TopToBottom] =
      CommonEncoder("flowchart TB")

  sealed trait BottomToTop extends Flowchart

  object BottomToTop:
    given MermaidDiagramEncoder[BottomToTop] =
      CommonEncoder("flowchart BT")

  sealed trait Common extends Flowchart, LeftToRight, RightToLeft, TopDown, TopToBottom, BottomToTop

  case class Node(id: String, text: Option[String]) extends Common:
    def withText(s: String): Node =
      copy(text = Some(s))

  object Node:
    case class Round(id: String, text: String) extends Common

    case class Stadium(id: String, text: String) extends Common

    case class Subroutine(id: String, text: String) extends Common

    case class Cylinder(id: String, text: String) extends Common

    case class Circle(id: String, text: String) extends Common

    case class Asymmetric(id: String, text: String) extends Common

    case class Rhombus(id: String, text: String) extends Common

    case class Hexagon(id: String, text: String) extends Common

    case class Parallelogram(id: String, text: String) extends Common

    case class ParallelogramAlt(id: String, text: String) extends Common

    case class Trapezoid(id: String, text: String) extends Common

    case class TrapezoidAlt(id: String, text: String) extends Common

    case class DoubleCircle(id: String, text: String) extends Common
