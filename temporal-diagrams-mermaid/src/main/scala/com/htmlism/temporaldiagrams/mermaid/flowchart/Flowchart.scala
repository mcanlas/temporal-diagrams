package com.htmlism.temporaldiagrams
package mermaid
package flowchart

import scala.util.chaining.*

import cats.Order.*
import cats.*
import cats.data.Chain
import cats.syntax.all.*

case class Flowchart(entities: Set[FlowchartDsl.Entity], links: Set[FlowchartDsl.Link]) extends FlowchartCommon

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

  def deriveMonoid[A <: FlowchartCommon](
      f: (Set[FlowchartDsl.Entity], Set[FlowchartDsl.Link]) => A
  ): Monoid[A] =
    new Monoid[A]:
      def empty: A =
        f(Set.empty, Set.empty)

      def combine(x: A, y: A): A =
        f(
          x.entities ++ y.entities,
          x.links ++ y.links
        )

  given Monoid[Flowchart] =
    deriveMonoid(Flowchart(_, _))

  private def renderSubsectionSorted[A: DiagramEncoder](xs: Set[A]) =
    xs.toList.map(summon[DiagramEncoder[A]].encode).sorted

  case class CommonEncoder[A <: FlowchartCommon](header: String) extends MermaidDiagramEncoder[A]:
    def encode(x: A): Chain[String] =
      Chain(
        x.entities.pipe(renderSubsectionSorted),
        x.links.pipe(renderSubsectionSorted)
      )
        .filter(_.nonEmpty)           // drop empty sections
        .map(Chain.fromSeq)           // from list to chain
        .flatten                      // make them one stream of bundles
        .pipe(interlace(_, identity)) // intersperse newlines and flatten

  given MermaidDiagramEncoder[Flowchart] =
    CommonEncoder("flowchart")

  case class LeftToRight(entities: Set[FlowchartDsl.Entity], links: Set[FlowchartDsl.Link]) extends FlowchartCommon

  object LeftToRight extends FlowchartFactory(LeftToRight(_, _)):
    given MermaidDiagramEncoder[LeftToRight] =
      CommonEncoder("flowchart LR")

    given Monoid[LeftToRight] =
      deriveMonoid(LeftToRight(_, _))

  case class RightToLeft(entities: Set[FlowchartDsl.Entity], links: Set[FlowchartDsl.Link]) extends FlowchartCommon

  object RightToLeft extends FlowchartFactory(RightToLeft(_, _)):
    given MermaidDiagramEncoder[RightToLeft] =
      CommonEncoder("flowchart RL")

    given Monoid[RightToLeft] =
      deriveMonoid(RightToLeft(_, _))

  case class TopDown(entities: Set[FlowchartDsl.Entity], links: Set[FlowchartDsl.Link]) extends FlowchartCommon

  object TopDown extends FlowchartFactory(TopDown(_, _)):
    given MermaidDiagramEncoder[TopDown] =
      CommonEncoder("flowchart TD")

    given Monoid[TopDown] =
      deriveMonoid(TopDown(_, _))

  case class TopToBottom(entities: Set[FlowchartDsl.Entity], links: Set[FlowchartDsl.Link]) extends FlowchartCommon

  object TopToBottom extends FlowchartFactory(TopToBottom(_, _)):
    given MermaidDiagramEncoder[TopToBottom] =
      CommonEncoder("flowchart TB")

    given Monoid[TopToBottom] =
      deriveMonoid(TopToBottom(_, _))

  case class BottomToTop(entities: Set[FlowchartDsl.Entity], links: Set[FlowchartDsl.Link]) extends FlowchartCommon

  object BottomToTop extends FlowchartFactory(BottomToTop(_, _)):
    given MermaidDiagramEncoder[BottomToTop] =
      CommonEncoder("flowchart BT")

    given Monoid[BottomToTop] =
      deriveMonoid(BottomToTop(_, _))
