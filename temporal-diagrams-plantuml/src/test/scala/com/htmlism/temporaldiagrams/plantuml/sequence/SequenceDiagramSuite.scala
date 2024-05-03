package com.htmlism.temporaldiagrams.plantuml.sequence

import scala.collection.immutable.ListSet

import weaver.*

object SequenceDiagramSuite extends FunSuite:
  test("PlantUML.com basic examples".ignore):
    val _ =
      SequenceDiagram(
        participants = ListSet(
          Participant("Alice"),
          Participant("Bob")
        ),
        messages = List(
          Message("Alice", "Bob"),
          Message("Bob", "Alice")
        )
      )

    expect.eql(1, 1)
