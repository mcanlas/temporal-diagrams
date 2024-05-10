package com.htmlism.temporaldiagrams
package plantuml
package sequence

import scala.collection.immutable.ListSet

import cats.data.Chain

case class SequenceDiagram(
    directives: Set[String]            = Set.empty,
    participants: ListSet[Participant] = ListSet.empty,
    messages: List[Message]            = Nil
)

object SequenceDiagram:
  given DiagramEncoder[SequenceDiagram] with
    def encode(x: SequenceDiagram): Chain[String] =
      PlantUml.asDocument:
        Chain.empty
