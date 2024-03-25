package com.htmlism.temporaldiagrams
package mermaid.flowchart

import scala.util.chaining.*

import cats.data.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.mermaid.flowchart

// https://mermaid.js.org/syntax/flowchart.html
sealed trait FlowchartDsl

object FlowchartDsl:
  sealed trait Declaration extends FlowchartDsl

  // https://mermaid.js.org/syntax/flowchart.html#styling-and-classes
  type StyleSpec =
    NonEmptyList[StyleDeclaration]

  object StyleSpec:
    def encode(s: StyleSpec): String =
      s
        .map(_.encode)
        .mkString_(", ")

  case class StyleDeclaration(property: String, value: String):
    def encode: String =
      property + ":" + value

  case class Subgraph(
      id: String,
      text: Option[String],
      direction: Option[Subgraph.Direction],
      declarations: Set[FlowchartDsl.Declaration],
      links: Set[FlowchartDsl.Link]
  ) extends FlowchartCommon
      with Declaration

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

  sealed trait Node extends Declaration

  object Declaration:
    given DiagramEncoder[Declaration] with
      def encode(x: Declaration): Chain[String] =
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

          case s: Style =>
            summon[DiagramEncoder[Style]].encode(s)

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

  /**
    * Defines a style for a specific node, identified by ID
    */
  case class Style(id: String, styles: NonEmptyList[(String, String)]) extends Declaration

  object Style:
    given DiagramEncoder[Style] with
      def encode(x: Style): Chain[String] =
        x match
          case Style(id, xs) =>
            val pairs = xs
              .map: (k, v) =>
                s"$k:$v"
              .mkString_(",")

            Chain.one:
              List(
                "style",
                id,
                pairs
              ).mkString(" ")

    def apply(id: String, x: (String, String), xs: (String, String)*): Style =
      Style(id, NonEmptyList(x, xs.toList))

  sealed trait Link extends FlowchartDsl

  object Link:
    /**
      * In it's most complex form, a link can have multiple sources and multiple segments chained together.
      *
      * @param sources
      *   A collection of sources that a link starts from
      * @param xs
      *   A collection of link styles and destinations that a link goes to
      */
    case class LinkChain(sources: NonEmptyList[String], xs: NonEmptyList[Segment]) extends Link

    enum Segment:
      case Visible(
          length: Int,
          weight: Weight,
          direction: Direction,
          text: Option[String],
          destinations: NonEmptyList[String],
          style: Option[StyleSpec]
      )
      case Invisible(length: Int, destinations: NonEmptyList[String], style: Option[StyleSpec])

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
