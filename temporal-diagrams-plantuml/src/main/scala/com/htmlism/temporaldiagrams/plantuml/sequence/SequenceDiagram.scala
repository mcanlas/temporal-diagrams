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
        Chain
          .fromSeq:
            x.participants.toList
          .flatMap(_.encode)

      PlantUml.asDocument:
        participants
