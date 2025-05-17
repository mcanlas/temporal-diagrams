package com.htmlism.temporaldiagrams
package mermaid.flowchart

import scala.util.chaining.*

import cats.data.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.mermaid.flowchart
import com.htmlism.temporaldiagrams.syntax.*

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
      text: Option[String]                        = None,
      direction: Option[Subgraph.Direction]       = None,
      declarations: Set[FlowchartDsl.Declaration] = Set.empty,
      links: Set[FlowchartDsl.Link]               = Set.empty
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
            n.encode

          case s: Style =>
            s.encode

          case cd: ClassDef =>
            cd.encode

          case ca: ClassAttachment =>
            ca.encode

  object Node:
    case class Simple(id: String, text: Option[String] = None, nodeClass: Option[String] = None) extends Node:
      def withText(s: String): Node =
        copy(text = s.some)

      def withClass(s: String): Node =
        copy(nodeClass = s.some)

    case class WithShape(id: String, shape: Shape, text: Option[String] = None, nodeClass: Option[String] = None)
        extends Node:
      def withText(s: String): WithShape =
        copy(text = s.some)

      def withClass(s: String): WithShape =
        copy(nodeClass = s.some)

    sealed abstract class Shape(val left: String, val right: String)

    object Shape:
      case object Square           extends Shape("[", "]")
      case object Round            extends Shape("(", ")")
      case object Stadium          extends Shape("([", "])")
      case object Subroutine       extends Shape("[[", "]]")
      case object Cylinder         extends Shape("[(", ")]")
      case object Circle           extends Shape("((", "))")
      case object Asymmetric       extends Shape(">", "]")
      case object Rhombus          extends Shape("{", "}")
      case object Hexagon          extends Shape("{{", "}}")
      case object Parallelogram    extends Shape("[/", "/]")
      case object ParallelogramAlt extends Shape("[\\", "\\]")
      case object Trapezoid        extends Shape("[/", "\\]")
      case object TrapezoidAlt     extends Shape("[\\", "/]")
      case object DoubleCircle     extends Shape("(((", ")))")

    given DiagramEncoder[Node] with
      def encode(x: Node): Chain[String] =
        x match
          case Node.Simple(id, oText, oClass) =>
            val text =
              oText.map(s => s"[$s]").getOrElse("")

            val classStr =
              oClass.map(":::" + _).getOrElse("")

            Chain.one:
              id + text + classStr

          case Node.WithShape(id, shape, oText, oClass) =>
            val text = oText.getOrElse(id)

            val classStr =
              oClass.map(":::" + _).getOrElse("")

            Chain.one:
              s"$id${shape.left}$text${shape.right}$classStr"

  /**
    * Defines a style for a specific node, identified by ID
    */
  case class Style(id: String, styles: NonEmptyList[(String, String)]) extends Declaration

  /**
    * Defines one or more classes and styles associated with them
    */
  case class ClassDef(ids: NonEmptyList[String], styles: NonEmptyList[(String, String)]) extends Declaration

  object ClassDef:
    given DiagramEncoder[ClassDef] with
      def encode(x: ClassDef): Chain[String] =
        x match
          case ClassDef(ids, styles) =>
            val pairs = styles
              .map: (k, v) =>
                s"$k:$v"
              .mkString_(",")

            val idsStr =
              ids.mkString_(",")

            Chain.one:
              List(
                "classDef",
                idsStr,
                pairs
              ).mkString(" ")

  /**
    * Defines an association between one (or more) nodes and a single class
    */
  case class ClassAttachment(ids: NonEmptyList[String], name: String) extends Declaration

  object ClassAttachment:
    given DiagramEncoder[ClassAttachment] with
      def encode(x: ClassAttachment): Chain[String] =
        x match
          case ClassAttachment(ids, name) =>
            val idsStr =
              ids.mkString_(",")

            Chain.one:
              List(
                "class",
                idsStr,
                name
              ).mkString(" ")

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
          destinations: NonEmptyList[String],
          text: Option[String]     = None,
          style: Option[StyleSpec] = None
      )
      case Invisible(length: Int, destinations: NonEmptyList[String], style: Option[StyleSpec] = None)

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
