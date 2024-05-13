package com.htmlism.temporaldiagrams.plantuml.sequence

import scala.collection.immutable.ListSet

import cats.syntax.all.*
import weaver.*

import com.htmlism.temporaldiagrams.syntax.*

object SequenceDiagramSuite extends FunSuite:
  test("PlantUML.com basic examples"):
    val expected =
      """@startuml
        |
        |participant Alice
        |participant Bob
        |Alice -> Bob: Authentication Request
        |Bob --> Alice: Authentication Response
        |Alice -> Bob: Another authentication Request
        |Bob --> Alice: Another authentication Response
        |
        |@enduml""".stripMargin

    val encoded =
      SequenceDiagram(
        participants = ListSet(
          Participant("Alice"),
          Participant("Bob")
        ),
        messages = List(
          Message("Alice", "Bob").withText("Authentication Request"),
          Message("Bob", "Alice", style = Message.Style.Dotted).withText("Authentication Response"),
          Message("Alice", "Bob").withText("Another authentication Request"),
          Message("Bob", "Alice", style = Message.Style.Dotted).withText("Another authentication Response")
        )
      )
        .encode
        .mkString_("\n")

    expect.eql(expected, encoded)

  test("PlantUML.com participants example"):
    expect.eql(1, 1)
