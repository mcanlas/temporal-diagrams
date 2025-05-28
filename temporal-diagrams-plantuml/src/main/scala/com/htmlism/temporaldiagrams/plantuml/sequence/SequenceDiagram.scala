package com.htmlism.temporaldiagrams
package plantuml
package sequence

import scala.collection.immutable.ListSet
import scala.util.chaining.*

import cats.data.Chain

import com.htmlism.temporaldiagrams.syntax.*

/**
  * @param directives
  * @param participants
  *   An ordered, unique sequence of participants. Rendered in order of (empty sort order; sort order lowest value;
  *   definition order)
  * @param messages
  */
case class SequenceDiagram(
    directives: Set[String]            = Set.empty,
    participants: ListSet[Participant] = ListSet.empty,
    messages: List[Message]            = Nil
)

object SequenceDiagram:
  given DiagramEncoder[SequenceDiagram] with
    def encode(x: SequenceDiagram): Chain[String] =
      val participantsLines =
        val basicParticipants =
          x.participants
            .toList
            .collect:
              case p: Participant.Basic => p

        if basicParticipants.length == x.participants.size then encodeBasicParticipantsVertically(basicParticipants)
        else
          x.participants
            .toList
            .pipe(Chain.fromSeq)
            .flatMap(encodeParticipant)

      val messagesLines =
        Chain
          .fromSeq:
            x.messages
          .flatMap(_.encode)

      PlantUml.asDocument:
        participantsLines ++ messagesLines

  def escapeText(s: String): String =
    if Set(" ", "(", ")").exists(s.contains) then s"\"$s\""
    else s

  private def encodeParticipant(p: Participant) =
    p match
      case Participant.Basic(name, shape, oAlias, oOrder, oColor) =>
        val aliasStr =
          oAlias.map(" as " + _).getOrElse("")

        val orderStr =
          oOrder.map(" order " + _.toString).getOrElse("")

        val colorStr =
          oColor.map(" " + _).getOrElse("")

        Chain
          .one:
            s"${shape.s} ${escapeText(name)}" + aliasStr + orderStr + colorStr

      case Participant.MultiLine(id, xs, oOrder) =>
        val orderStr =
          oOrder.map(" order " + _.toString).getOrElse("")

        Chain
          .fromSeq(xs)
          .map("  " + _)
          .prepend(s"participant $id$orderStr [")
          .append("]")

  private def encodeBasicParticipantsVertically(xs: List[Participant.Basic]): Chain[String] =
    val parts =
      xs.map: p =>
        val aliasStr =
          p.alias.map(" as " + _).getOrElse("")

        val orderStr =
          p.order.map(" order " + _.toString).getOrElse("")

        val colorStr =
          p.color.map(" " + _).getOrElse("")

        (p.shape.s, p.name, aliasStr, orderStr, colorStr)

    val shapeWidth =
      parts.map(_._1.length).max

    val idWidth =
      parts.map(t => escapeText(t._2).length).max

    val aliasWidth =
      parts.map(_._3.length).max

    val orderWidth =
      parts.map(_._4.length).max

    Chain
      .fromSeq:
        parts
          .map: (shape, id, alias, order, color) =>
            List(
              s"%-${shapeWidth}s %-${idWidth}s".formatted(shape, escapeText(id)),
              if aliasWidth > 0 then s"%-${aliasWidth}s".formatted(alias) else "",
              if orderWidth > 0 then s"%-${orderWidth}s".formatted(order) else "",
              color
            ).mkString("").trim
