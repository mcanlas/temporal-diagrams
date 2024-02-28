package com.htmlism.temporaldiagrams.mermaid

import cats.data.Chain

/**
  * @tparam A
  *   The specific diagram type
  */
case class MermaidDiagram[A](frontmatter: Chain[FrontmatterPair], xs: Chain[A])

object MermaidDiagram:
  def empty[A]: MermaidDiagram[A] =
    MermaidDiagram(Chain.empty, Chain.empty)

  /**
    * @tparam A
    *   The specific diagram type
    */
  def render[A: MermaidDiagramType](x: MermaidDiagram[A]): Chain[String] =
    Chain.one:
      summon[MermaidDiagramType[A]].header
