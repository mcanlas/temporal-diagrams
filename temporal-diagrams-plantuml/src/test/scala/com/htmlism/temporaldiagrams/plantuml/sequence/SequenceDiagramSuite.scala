package com.htmlism.temporaldiagrams.plantuml.sequence

import scala.collection.immutable.ListSet

import weaver.*

import com.htmlism.temporaldiagrams.syntax.*

object SequenceDiagramSuite extends FunSuite:
  test("PlantUML.com basic examples"):
    val d =
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

    println(d.encode)

    expect.eql(1, 1)
