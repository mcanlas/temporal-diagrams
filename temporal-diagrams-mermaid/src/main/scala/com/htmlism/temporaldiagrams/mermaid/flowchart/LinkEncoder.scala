package com.htmlism.temporaldiagrams.mermaid.flowchart

import cats.data.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl.Link.*

/**
  * To support styling, links are implicit given IDs (starting from zero)
  *
  * https://mermaid.js.org/syntax/flowchart.html#styling-links
  *
  * This encoder handles assignment of these IDs
  */
object LinkEncoder:
  def encode(xs: Set[FlowchartDsl.Link]): List[Chain[String]] =
    xs
      .toList
      .map(encodeOne)
      .sortBy(_._1)
      .map(t => Chain.one(t._1))

  def encodeOne(x: FlowchartDsl.Link): (String, NonEmptyChain[Option[Int => String]]) =
    x match
      case LinkChain(srcs, xs) =>
        val sourcePart =
          ampersand(srcs)

        val destinationParts =
          xs
            .toList
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
            .separate
            ._1
            .flatten

        val linkStr =
          (sourcePart :: destinationParts)
            .mkString(" ")

        linkStr -> NonEmptyChain.one(None)

  private def ampersand(xs: NonEmptyList[String]) =
    xs.mkString_(" & ")
