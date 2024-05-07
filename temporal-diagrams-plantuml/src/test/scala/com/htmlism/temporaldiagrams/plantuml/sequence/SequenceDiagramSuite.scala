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
          Message("Alice", "Bob").withText("Authentication Request"),
          Message("Bob", "Alice", style = Message.Style.Dotted).withText("Authentication Response")
        )
      )

    expect.eql(1, 1)
