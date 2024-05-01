package com.htmlism.temporaldiagrams.plantuml.sequence

import scala.collection.immutable.ListSet

case class SequenceDiagram(
    directives: Set[String],
    participants: ListSet[Participant],
    messages: Set[Message]
)
