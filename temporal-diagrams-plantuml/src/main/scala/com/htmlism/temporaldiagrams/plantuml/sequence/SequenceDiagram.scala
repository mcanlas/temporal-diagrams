package com.htmlism.temporaldiagrams
package plantuml.sequence

import scala.collection.immutable.ListSet

case class SequenceDiagram(
    directives: Set[String]            = Set.empty,
    participants: ListSet[Participant] = ListSet.empty,
    messages: List[Message]            = Nil
)

object SequenceDiagram:
  given DiagramEncoder[SequenceDiagram] = ???
