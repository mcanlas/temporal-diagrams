package com.htmlism.temporaldiagrams
package mermaid.flowchart

import scala.util.chaining.*

import cats.data.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl.Link.LinkChain.Segment.Visible

// https://mermaid.js.org/syntax/flowchart.html
sealed trait FlowchartDsl

object FlowchartDsl:
  sealed trait Entity extends FlowchartDsl

  // https://mermaid.js.org/syntax/flowchart.html#styling-and-classes
  type StyleSpec =
    NonEmptyList[StyleDeclaration]

  case class StyleDeclaration(property: String, value: String)

  case class Subgraph(
      id: String,
      text: Option[String],
      direction: Option[Subgraph.Direction],
      entities: Set[FlowchartDsl.Entity],
      links: Set[FlowchartDsl.Link]
  ) extends FlowchartCommon
      with Entity

  object Subgraph:
    sealed abstract class Direction(val declaration: String)

    // interestingly, TD not supported unlike flowchart
    object Direction:
      val LR = LeftToRight
      val RL = RightToLeft
      val TB = TopToBottom
      val BT = BottomToTop

      object LeftToRight extends Direction("LR")
      object RightToLeft extends Direction("RL")
      object TopToBottom extends Direction("TB")
      object BottomToTop extends Direction("BT")

  sealed trait Node extends Entity

  object Entity:
    given DiagramEncoder[Entity] with
      def encode(x: Entity): Chain[String] =
        x match
          case sg @ Subgraph(id, oText, oDir, _, _) =>
            val directionXs =
              oDir
                .map(s => "direction " + s.declaration)
                .toList
                .pipe(Chain.fromSeq)

            val body =
              (directionXs ++ FlowchartCommon
                .encode(sg))
                .map: s =>
                  if s.isEmpty then s
                  else s"  $s"

            val text =
              oText
                .map(s => s" [$s]")
                .getOrElse("")

            body
              .prepend(s"subgraph $id$text")
              .append("end")

          case n: Node =>
            summon[DiagramEncoder[Node]].encode(n)

  object Node:
    private def encodeNode(left: String, right: String)(id: String, text: String) =
      Chain.one:
        s"$id$left$text$right"

    given DiagramEncoder[Node] with
      def encode(x: Node): Chain[String] =
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

  sealed trait Link extends FlowchartDsl

  object Link:
    private def ampersand(xs: NonEmptyList[String]) =
      xs.mkString_(" & ")

    given DiagramEncoder[Link] with
      def encode(x: Link): Chain[String] =
        x match
          case LinkChain(srcs, xs) =>
            val sourcePart =
              ampersand(srcs)

            val destinationParts =
              xs
                .toList
                .flatMap:
                  case LinkChain.Segment.Invisible(length, destinations) =>
                    val body =
                      "~" * (length + 2)

                    List(
                      body,
                      ampersand(destinations)
                    )

                  case LinkChain.Segment.Visible(length, weight, direction, oText, destinations) =>
                    val (leftHead, rightHead) =
                      (weight, direction) match
                        case (Weight.Normal, Direction.Open) =>
                          "" -> "-"

                        case (Weight.Dotted, Direction.Open) =>
                          "" -> ""

                        case (Weight.Thick, Direction.Open) =>
                          "" -> "="

                        case (_, Direction.Single(Head.Arrow)) =>
                          "" -> ">"

                        case (_, Direction.Single(Head.Circle)) =>
                          "" -> "o"

                        case (_, Direction.Single(Head.Cross)) =>
                          "" -> "x"

                        case (_, Direction.Multi(Head.Arrow)) =>
                          "<" -> ">"

                        case (_, Direction.Multi(Head.Circle)) =>
                          "o" -> "o"

                        case (_, Direction.Multi(Head.Cross)) =>
                          "x" -> "x"

                    val leftBody =
                      weight match
                        case Weight.Normal =>
                          "--"

                        case Weight.Dotted =>
                          "-."

                        case Weight.Thick =>
                          "=="

                    val rightBody =
                      (oText, weight) match
                        case (Some(s), Weight.Normal) =>
                          s" $s " + ("-" * (length + 1))

                        case (Some(s), Weight.Dotted) =>
                          s" $s " + ("." * length) + "-"

                        case (Some(s), Weight.Thick) =>
                          s" $s " + ("=" * (length + 1))

                        case (None, Weight.Normal) =>
                          "-" * (length - 1)

                        case (None, Weight.Dotted) =>
                          ("." * (length - 1)) + "-"

                        case (None, Weight.Thick) =>
                          "=" * (length - 1)

                    val body =
                      leftHead + leftBody + rightBody + rightHead

                    List(
                      body,
                      ampersand(destinations)
                    )

            Chain.one:
              (sourcePart :: destinationParts)
                .mkString(" ")

    case class LinkChain(sources: NonEmptyList[String], xs: NonEmptyList[LinkChain.Segment]) extends Link

    object LinkChain:
      enum Segment:
        case Visible(
            length: Int,
            weight: Weight,
            direction: Direction,
            text: Option[String],
            destinations: NonEmptyList[String]
        )
        case Invisible(length: Int, destinations: NonEmptyList[String])

    enum Weight:
      case Normal
      case Dotted
      case Thick

    enum Head:
      case Arrow
      case Circle
      case Cross

    enum Direction:
      case Open
      case Single(head: Head)
      case Multi(head: Head)
