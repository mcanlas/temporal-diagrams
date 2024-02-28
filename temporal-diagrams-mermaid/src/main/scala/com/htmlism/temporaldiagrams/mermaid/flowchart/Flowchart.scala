package com.htmlism.temporaldiagrams.mermaid
package flowchart

sealed trait Flowchart

object Flowchart:
  type LR = LeftToRight
  type RL = RightToLeft
  type TD = TopDown
  type TB = TopToBottom
  type BT = BottomToTop

  given MermaidDiagramType[Flowchart] with
    def header: String =
      "flowchart"

  sealed trait LeftToRight extends Flowchart

  object LeftToRight:
    given MermaidDiagramType[LeftToRight] with
      def header: String =
        "flowchart LR"

  case class Node(id: String, text: Option[String]) extends Flowchart
