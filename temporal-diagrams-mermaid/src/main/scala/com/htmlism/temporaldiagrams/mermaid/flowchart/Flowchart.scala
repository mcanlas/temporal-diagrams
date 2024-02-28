package com.htmlism.temporaldiagrams
package mermaid
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

object Flowchart extends FlowchartFactory(Flowchart(_, _)):
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

  def deriveMonoid[A <: FlowchartCommon](
      f: (Set[FlowchartDeclaration.Node], Set[FlowchartDeclaration.Link]) => A
  ): Monoid[A] =
    new Monoid[A]:
      def empty: A =
        f(Set.empty, Set.empty)

      def combine(x: A, y: A): A =
        f(
          x.nodes ++ y.nodes,
          x.links ++ y.links
        )

  given Monoid[Flowchart] =
    deriveMonoid(Flowchart(_, _))

  private def renderSubsectionSorted[A: DiagramEncoder](xs: Set[A]) =
    xs.toList.map(summon[DiagramEncoder[A]].encode).sorted

  case class CommonEncoder[A <: FlowchartCommon](header: String) extends MermaidDiagramEncoder[A]:
    def encode(x: A): Chain[String] =
      x
        .nodes
        .pipe(renderSubsectionSorted)
        .pipe(Chain.fromSeq)
        .flatMap(identity)

  given MermaidDiagramEncoder[Flowchart] =
    CommonEncoder("flowchart")

  case class LeftToRight(nodes: Set[FlowchartDeclaration.Node], links: Set[FlowchartDeclaration.Link])
      extends FlowchartCommon

  object LeftToRight extends FlowchartFactory(LeftToRight(_, _)):
    given MermaidDiagramEncoder[LeftToRight] =
      CommonEncoder("flowchart LR")

    given Monoid[LeftToRight] =
      deriveMonoid(LeftToRight(_, _))

  case class RightToLeft(nodes: Set[FlowchartDeclaration.Node], links: Set[FlowchartDeclaration.Link])
      extends FlowchartCommon

  object RightToLeft extends FlowchartFactory(RightToLeft(_, _)):
    given MermaidDiagramEncoder[RightToLeft] =
      CommonEncoder("flowchart RL")

    given Monoid[RightToLeft] =
      deriveMonoid(RightToLeft(_, _))

  case class TopDown(nodes: Set[FlowchartDeclaration.Node], links: Set[FlowchartDeclaration.Link])
      extends FlowchartCommon

  object TopDown extends FlowchartFactory(TopDown(_, _)):
    given MermaidDiagramEncoder[TopDown] =
      CommonEncoder("flowchart TD")

    given Monoid[TopDown] =
      deriveMonoid(TopDown(_, _))

  case class TopToBottom(nodes: Set[FlowchartDeclaration.Node], links: Set[FlowchartDeclaration.Link])
      extends FlowchartCommon

  object TopToBottom extends FlowchartFactory(TopToBottom(_, _)):
    given MermaidDiagramEncoder[TopToBottom] =
      CommonEncoder("flowchart TB")

    given Monoid[TopToBottom] =
      deriveMonoid(TopToBottom(_, _))

  case class BottomToTop(nodes: Set[FlowchartDeclaration.Node], links: Set[FlowchartDeclaration.Link])
      extends FlowchartCommon

  object BottomToTop extends FlowchartFactory(BottomToTop(_, _)):
    given MermaidDiagramEncoder[BottomToTop] =
      CommonEncoder("flowchart BT")

    given Monoid[BottomToTop] =
      deriveMonoid(BottomToTop(_, _))
