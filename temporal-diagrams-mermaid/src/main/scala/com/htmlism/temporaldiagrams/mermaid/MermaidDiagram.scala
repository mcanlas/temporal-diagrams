package com.htmlism.temporaldiagrams.mermaid

import cats.*
import cats.data.Chain
import cats.syntax.all.*

/**
  * @tparam A
  *   The specific diagram type
  */
case class MermaidDiagram[A](frontmatter: Chain[FrontmatterPair], diagram: A)

object MermaidDiagram:
  given [A](using A: Monoid[A]): Monoid[MermaidDiagram[A]] with
    def empty: MermaidDiagram[A] =
      MermaidDiagram.empty

    def combine(x: MermaidDiagram[A], y: MermaidDiagram[A]) =
      MermaidDiagram(
        x.frontmatter ++ y.frontmatter,
        x.diagram |+| y.diagram
      )

  def empty[A](using A: Monoid[A]): MermaidDiagram[A] =
    MermaidDiagram(Chain.empty, A.empty)

  /**
    * @tparam A
    *   The specific diagram type
    */
  def render[A](x: MermaidDiagram[A])(using A: MermaidDiagramEncoder[A]): Chain[String] =
    val frontmatterLines =
      if x.frontmatter.isEmpty then Chain.empty
      else
        x
          .frontmatter
          .flatMap(FrontmatterPair.encode)
          .prepend("---")
          .append("---")

    val headerLines =
      Chain.one:
        A.header

    val bodyLines =
      A
        .encode(x.diagram)
        .map: s =>
          if s.isEmpty then s
          else s"  $s"

    frontmatterLines ++ headerLines ++ bodyLines
