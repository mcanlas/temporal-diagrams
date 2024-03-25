package com.htmlism.temporaldiagrams
package mermaid.flowchart

import scala.util.chaining.*

import cats.Order.*
import cats.data.Chain
import cats.syntax.all.*

/**
  * Inputs for rendering shared by top-level flowcharts and sub-graphs
  */
trait FlowchartCommon:
  def declarations: Set[FlowchartDsl.Declaration]
  def links: Set[FlowchartDsl.Link]

object FlowchartCommon:
  def encode(x: FlowchartCommon): Chain[String] =
    val enc =
      summon[DiagramEncoder[FlowchartDsl.Declaration]]

    val encodedDeclarations =
      x
        .declarations
        .iterator
        .map:
          case x: FlowchartDsl.Node =>
            0 -> x
          case x =>
            1 -> x
        .map: (n, x) =>
          n -> enc.encode(x)
        .toList
        .sorted
        .map: (_, str) =>
          str

    Chain(
      encodedDeclarations,
      x.links.pipe(LinkEncoder.encode)
    )
      .filter(_.nonEmpty)           // drop empty sections
      .map(Chain.fromSeq)           // from list to chain
      .flatten                      // make them one stream of bundles
      .pipe(interlace(_, identity)) // intersperse newlines and flatten
