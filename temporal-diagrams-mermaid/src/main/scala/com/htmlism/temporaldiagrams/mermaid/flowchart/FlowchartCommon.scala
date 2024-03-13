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
  private def renderSubsectionSorted[A](f: A => Chain[String])(xs: Set[A]) =
    xs.toList.map(f).sorted

  def encode(x: FlowchartCommon): Chain[String] =
    Chain(
      x.entities.pipe(renderSubsectionSorted(summon[DiagramEncoder[FlowchartDsl.Entity]].encode)),
      x.links.pipe(LinkEncoder.encode)
    )
      .filter(_.nonEmpty)           // drop empty sections
      .map(Chain.fromSeq)           // from list to chain
      .flatten                      // make them one stream of bundles
      .pipe(interlace(_, identity)) // intersperse newlines and flatten
