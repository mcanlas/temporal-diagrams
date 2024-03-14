package com.htmlism.temporaldiagrams.mermaid.flowchart

import scala.util.chaining.*

import cats.data.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl.Link.*
import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl.StyleSpec

/**
  * To support styling, links are implicit given IDs (starting from zero)
  *
  * https://mermaid.js.org/syntax/flowchart.html#styling-links
  *
  * This encoder handles assignment of these IDs
  */
object LinkEncoder:
  /**
    * An entire diagram's worth of links must be encoded at the same time because they are assigned IDs, starting from
    * zero, in the order of their declaration
    */
  def encode(xs: Set[FlowchartDsl.Link]): List[Chain[String]] =
    val sortedLinks =
      xs
        .toList
        .map(partiallyEncode)
        .sortBy(_._1)

    sortedLinks
      .traverse: (s, styles) =>
        State: (iLink: Int) =>
          val styleLines =
            styles
              .zipWithIndex
              .toList
              .flatMap: (oStyle, iStyle) =>
                oStyle.map(f => f(iLink + iStyle)).toList
              .pipe(Chain.fromSeq)

          val linkAndStyleLinks =
            Chain.one(s) ++ styleLines

          iLink + styles.size -> linkAndStyleLinks
      .run(0)
      .value
      ._2

  /**
    * Encodes the link DSL to a string, and also provides a collection of (optional) style declarations that need their
    * ID injected. The collection is non-empty because each link will consume an ID, even if it does not have a style
    */
  private def partiallyEncode(x: FlowchartDsl.Link): (String, NonEmptyList[Option[Int => String]]) =
    x match
      case LinkChain(srcs, xs) =>
        val sourcePart =
          ampersand(srcs)

        val destinationPartsAndStyles =
          xs
            .map:
              case LinkChain.Segment.Invisible(length, destinations, style) =>
                val body =
                  "~" * (length + 2)

                List(
                  body,
                  ampersand(destinations)
                ) -> style

              case LinkChain.Segment.Visible(length, weight, direction, oText, destinations, style) =>
                val (leftHead, rightHead) =
                  (weight, direction) match
                    case (Weight.Normal, Direction.Open) =>
                      "" -> "-"

                    case (Weight.Dotted, Direction.Open) =>
                      "" -> ""

                    case (Weight.Thick, Direction.Open) =>
                      "" -> "="

                    case (_, Direction.Single(Head.Arrow)) =>
                      "" -> ">"

                    case (_, Direction.Single(Head.Circle)) =>
                      "" -> "o"

                    case (_, Direction.Single(Head.Cross)) =>
                      "" -> "x"

                    case (_, Direction.Multi(Head.Arrow)) =>
                      "<" -> ">"

                    case (_, Direction.Multi(Head.Circle)) =>
                      "o" -> "o"

                    case (_, Direction.Multi(Head.Cross)) =>
                      "x" -> "x"

                val leftBody =
                  weight match
                    case Weight.Normal =>
                      "--"

                    case Weight.Dotted =>
                      "-."

                    case Weight.Thick =>
                      "=="

                val rightBody =
                  (oText, weight) match
                    case (Some(s), Weight.Normal) =>
                      s" $s " + ("-" * (length + 1))

                    case (Some(s), Weight.Dotted) =>
                      s" $s " + ("." * length) + "-"

                    case (Some(s), Weight.Thick) =>
                      s" $s " + ("=" * (length + 1))

                    case (None, Weight.Normal) =>
                      "-" * (length - 1)

                    case (None, Weight.Dotted) =>
                      ("." * (length - 1)) + "-"

                    case (None, Weight.Thick) =>
                      "=" * (length - 1)

                val body =
                  leftHead + leftBody + rightBody + rightHead

                List(
                  body,
                  ampersand(destinations)
                ) -> style

        val destinationParts =
          destinationPartsAndStyles
            .map(_._1)
            .toList
            .flatten

        val styles =
          destinationPartsAndStyles
            .map(_._2)
            .map(o =>
              o.map(ss =>
                (n: Int) =>
                  List("linkStyle", n, StyleSpec.encode(ss))
                    .mkString(" ")
              )
            )

        val linkStr =
          (sourcePart :: destinationParts)
            .mkString(" ")

        linkStr -> styles

  private def ampersand(xs: NonEmptyList[String]) =
    xs.mkString_(" & ")
