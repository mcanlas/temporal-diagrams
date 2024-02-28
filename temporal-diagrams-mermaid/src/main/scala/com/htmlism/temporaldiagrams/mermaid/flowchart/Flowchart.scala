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

  sealed trait LeftToRight

  object LeftToRight:
    given MermaidDiagramType[LeftToRight] with
      def header: String =
        "flowchart LR"

  sealed trait RightToLeft

  object RightToLeft:
    given MermaidDiagramType[RightToLeft] with
      def header: String =
        "flowchart RL"

  sealed trait TopDown

  object TopDown:
    given MermaidDiagramType[TopDown] with
      def header: String =
        "flowchart TD"

  sealed trait TopToBottom

  object TopToBottom:
    given MermaidDiagramType[TopToBottom] with
      def header: String =
        "flowchart TB"

  sealed trait BottomToTop

  object BottomToTop:
    given MermaidDiagramType[BottomToTop] with
      def header: String =
        "flowchart BT"

  sealed trait Common extends Flowchart, LeftToRight, RightToLeft, TopDown, TopToBottom, BottomToTop

  case class Node(id: String, text: Option[String]) extends Common
