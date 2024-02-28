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

  case class CommonEncoder[A <: Flowchart](s: String) extends MermaidDiagramEncoder[A]:
    def header: String =
      s

    def encode(x: A): Chain[String] =
      Chain.empty

  given MermaidDiagramEncoder[Flowchart] =
    CommonEncoder("flowchart")

  sealed trait LeftToRight extends Flowchart

  object LeftToRight:
    given MermaidDiagramEncoder[LeftToRight] =
      CommonEncoder("flowchart LR")

  sealed trait RightToLeft extends Flowchart

  object RightToLeft:
    given MermaidDiagramEncoder[RightToLeft] =
      CommonEncoder("flowchart RL")

  sealed trait TopDown extends Flowchart

  object TopDown:
    given MermaidDiagramEncoder[TopDown] =
      CommonEncoder("flowchart TD")

  sealed trait TopToBottom extends Flowchart

  object TopToBottom:
    given MermaidDiagramEncoder[TopToBottom] =
      CommonEncoder("flowchart TB")

  sealed trait BottomToTop extends Flowchart

  object BottomToTop:
    given MermaidDiagramEncoder[BottomToTop] =
      CommonEncoder("flowchart BT")

  sealed trait Common extends Flowchart, LeftToRight, RightToLeft, TopDown, TopToBottom, BottomToTop

  case class Node(id: String, text: Option[String]) extends Common
