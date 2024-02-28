package com.htmlism.temporaldiagrams.mermaid
package flowchart

import scala.util.chaining.*

import cats.Order.*
import cats.*
import cats.data.Chain

trait FlowchartCommon:
  def nodes: Set[FlowchartDeclaration.Node]
  def links: Set[FlowchartDeclaration.Link]

case class Flowchart(nodes: Set[FlowchartDeclaration.Node], links: Set[FlowchartDeclaration.Link])
    extends FlowchartCommon

object Flowchart:
  type LR = LeftToRight
  type RL = RightToLeft
  type TD = TopDown
  type TB = TopToBottom
  type BT = BottomToTop

  val LR = LeftToRight
  val RL = RightToLeft
  val TD = TopDown
  val TB = TopToBottom
  val BT = BottomToTop

  val Node = FlowchartDeclaration.Node

  def empty: Flowchart =
    Flowchart(Set.empty, Set.empty)

  def apply(xs: FlowchartDeclaration*): Flowchart =
    Flowchart(
      xs.collect { case x: FlowchartDeclaration.Node => x }.toSet,
      Set.empty // TODO,
    )

  given Monoid[Flowchart] with
    def empty: Flowchart =
      Flowchart.empty

    def combine(x: Flowchart, y: Flowchart): Flowchart =
      Flowchart(
        x.nodes ++ y.nodes,
        x.links ++ y.links
      )

  case class CommonEncoder[A <: FlowchartCommon](s: String) extends MermaidDiagramEncoder[A]:
    def header: String =
      s

    def encode(x: A): Chain[String] =
      x
        .nodes
        .map(FlowchartDeclaration.encode)
        .toList
        .sorted
        .pipe(Chain.fromSeq)
        .flatMap(identity)

  given MermaidDiagramEncoder[Flowchart] =
    CommonEncoder("flowchart")

  case class LeftToRight(nodes: Set[FlowchartDeclaration.Node], links: Set[FlowchartDeclaration.Link])
      extends FlowchartCommon

  object LeftToRight:
    given MermaidDiagramEncoder[LeftToRight] =
      CommonEncoder("flowchart LR")

    given Monoid[LeftToRight] with
      def empty: LeftToRight =
        LeftToRight(Set.empty, Set.empty)

      def combine(x: LeftToRight, y: LeftToRight): LeftToRight =
        LeftToRight(
          x.nodes ++ y.nodes,
          x.links ++ y.links
        )

  case class RightToLeft(nodes: Set[FlowchartDeclaration.Node], links: Set[FlowchartDeclaration.Link])
      extends FlowchartCommon

  object RightToLeft:
    given MermaidDiagramEncoder[RightToLeft] =
      CommonEncoder("flowchart RL")

    given Monoid[RightToLeft] with
      def empty: RightToLeft =
        RightToLeft(Set.empty, Set.empty)

      def combine(x: RightToLeft, y: RightToLeft): RightToLeft =
        RightToLeft(
          x.nodes ++ y.nodes,
          x.links ++ y.links
        )

  case class TopDown(nodes: Set[FlowchartDeclaration.Node], links: Set[FlowchartDeclaration.Link])
      extends FlowchartCommon

  object TopDown:
    given MermaidDiagramEncoder[TopDown] =
      CommonEncoder("flowchart TD")

    given Monoid[TopDown] with
      def empty: TopDown =
        TopDown(Set.empty, Set.empty)

      def combine(x: TopDown, y: TopDown): TopDown =
        TopDown(
          x.nodes ++ y.nodes,
          x.links ++ y.links
        )

  case class TopToBottom(nodes: Set[FlowchartDeclaration.Node], links: Set[FlowchartDeclaration.Link])
      extends FlowchartCommon

  object TopToBottom:
    given MermaidDiagramEncoder[TopToBottom] =
      CommonEncoder("flowchart TB")

    given Monoid[TopToBottom] with
      def empty: TopToBottom =
        TopToBottom(Set.empty, Set.empty)

      def combine(x: TopToBottom, y: TopToBottom): TopToBottom =
        TopToBottom(
          x.nodes ++ y.nodes,
          x.links ++ y.links
        )

  case class BottomToTop(nodes: Set[FlowchartDeclaration.Node], links: Set[FlowchartDeclaration.Link])
      extends FlowchartCommon

  object BottomToTop:
    given MermaidDiagramEncoder[BottomToTop] =
      CommonEncoder("flowchart BT")

    given Monoid[BottomToTop] with
      def empty: BottomToTop =
        BottomToTop(Set.empty, Set.empty)

      def combine(x: BottomToTop, y: BottomToTop): BottomToTop =
        BottomToTop(
          x.nodes ++ y.nodes,
          x.links ++ y.links
        )
