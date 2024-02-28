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

  case class CommonEncoder[A](s: String) extends MermaidDiagramEncoder[A]:
    def header: String =
      s

    def encode(x: A): Chain[String] =
      Chain.empty

  given MermaidDiagramEncoder[Flowchart] =
    CommonEncoder("flowchart")

  sealed trait LeftToRight

  object LeftToRight:
    given MermaidDiagramEncoder[LeftToRight] =
      CommonEncoder("flowchart LR")

  sealed trait RightToLeft

  object RightToLeft:
    given MermaidDiagramEncoder[RightToLeft] =
      CommonEncoder("flowchart RL")

  sealed trait TopDown

  object TopDown:
    given MermaidDiagramEncoder[TopDown] =
      CommonEncoder("flowchart TD")

  sealed trait TopToBottom

  object TopToBottom:
    given MermaidDiagramEncoder[TopToBottom] =
      CommonEncoder("flowchart TB")

  sealed trait BottomToTop

  object BottomToTop:
    given MermaidDiagramEncoder[BottomToTop] =
      CommonEncoder("flowchart BT")

  sealed trait Common extends Flowchart, LeftToRight, RightToLeft, TopDown, TopToBottom, BottomToTop

  case class Node(id: String, text: Option[String]) extends Common
