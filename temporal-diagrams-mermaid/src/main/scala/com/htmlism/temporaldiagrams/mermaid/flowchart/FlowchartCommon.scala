package com.htmlism.temporaldiagrams
package mermaid.flowchart

import scala.util.chaining.*

import cats.Order.*
import cats.data.Chain
import cats.syntax.all.*

trait FlowchartCommon:
  def entities: Set[FlowchartDsl.Entity]
  def links: Set[FlowchartDsl.Link]

object FlowchartCommon:
  private def renderSubsectionSorted[A: DiagramEncoder](xs: Set[A]) =
    xs.toList.map(summon[DiagramEncoder[A]].encode).sorted

  def encode(x: FlowchartCommon): Chain[String] =
    Chain(
      x.entities.pipe(renderSubsectionSorted),
      x.links.pipe(renderSubsectionSorted)
    )
      .filter(_.nonEmpty)           // drop empty sections
      .map(Chain.fromSeq)           // from list to chain
      .flatten                      // make them one stream of bundles
      .pipe(interlace(_, identity)) // intersperse newlines and flatten
