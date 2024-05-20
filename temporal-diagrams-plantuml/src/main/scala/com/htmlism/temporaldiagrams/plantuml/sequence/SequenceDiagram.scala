package com.htmlism.temporaldiagrams
package plantuml
package sequence

import scala.collection.immutable.ListSet

import cats.data.Chain

import com.htmlism.temporaldiagrams.syntax.*

case class SequenceDiagram(
    directives: Set[String]            = Set.empty,
    participants: ListSet[Participant] = ListSet.empty,
    messages: List[Message]            = Nil
)

object SequenceDiagram:
  given DiagramEncoder[SequenceDiagram] with
    def encode(x: SequenceDiagram): Chain[String] =
      val participants =
        encodeParticipants:
          x.participants.toList

      val messages =
        Chain
          .fromSeq:
            x.messages
          .flatMap(_.encode)

      PlantUml.asDocument:
        participants ++ messages

  private def encodeParticipants(xs: List[Participant]): Chain[String] =
    val parts =
      xs.map: p =>
        val aliasStr =
          p.name.map(" as " + _).getOrElse("")

        val orderStr =
          p.order.map(" order " + _.toString).getOrElse("")

        val colorStr =
          p.color.map(" " + _).getOrElse("")

        (p.shape.s, p.id, aliasStr, orderStr, colorStr)

    val shapeWidth =
      parts.map(_._1.length).max

    val idWidth =
      parts.map(_._2.length).max

    val aliasWidth =
      parts.map(_._3.length).max

    val orderWidth =
      parts.map(_._4.length).max

    Chain
      .fromSeq:
        parts
          .map: (shape, id, alias, order, color) =>
            List(
              s"%-${shapeWidth}s %-${idWidth}s".formatted(shape, id),
              if aliasWidth > 0 then s"%-${aliasWidth}s".formatted(alias) else "",
              if orderWidth > 0 then s"%-${orderWidth}s".formatted(order) else "",
              color
            ).mkString("").trim
