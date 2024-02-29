package com.htmlism.temporaldiagrams
package mermaid
package flowchart

import scala.util.chaining.*

import cats.*
import cats.data.Chain

case class Flowchart(
    direction: Option[Flowchart.Direction],
    entities: Set[FlowchartDsl.Entity],
    links: Set[FlowchartDsl.Link]
) extends FlowchartCommon:
  def withDirection(dir: Flowchart.Direction): Flowchart =
    copy(direction = Some(dir))

object Flowchart:
  def empty: Flowchart =
    Flowchart(None, Set.empty, Set.empty)

  def apply(xs: FlowchartDsl*): Flowchart =
    Flowchart(
      None,
      xs.collect { case x: FlowchartDsl.Entity => x }.toSet,
      xs.collect { case x: FlowchartDsl.Link => x }.toSet
    )

  sealed abstract class Direction(val declaration: String)

  object Direction:
    val LR = LeftToRight
    val RL = RightToLeft
    val TD = TopDown
    val TB = TopToBottom
    val BT = BottomToTop

    object LeftToRight extends Direction("LR")
    object RightToLeft extends Direction("RL")
    object TopDown     extends Direction("TD")
    object TopToBottom extends Direction("TB")
    object BottomToTop extends Direction("BT")

  given Monoid[Flowchart] with
    def empty: Flowchart =
      Flowchart(None, Set.empty, Set.empty)

    // interesting departure from usual monoid affair, always takes whatever the right hand direction is
    def combine(x: Flowchart, y: Flowchart): Flowchart =
      Flowchart(
        y.direction,
        x.entities ++ y.entities,
        x.links ++ y.links
      )

  given MermaidDiagramEncoder[Flowchart] with
    def header(x: Flowchart): String =
      val directionStr =
        x.direction
          .map(" " + _.declaration)
          .getOrElse("")

      s"flowchart$directionStr"

    def encode(x: Flowchart): Chain[String] =
      FlowchartCommon.encode(x)
