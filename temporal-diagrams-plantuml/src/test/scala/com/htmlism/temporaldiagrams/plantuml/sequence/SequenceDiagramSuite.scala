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

  // TODO vertical formatting for participants
  test("PlantUML.com participants example".ignore):
    val expected =
      """@startuml
        |
        |participant Participant as Foo
        |actor       Actor       as Foo1
        |boundary    Boundary    as Foo2
        |control     Control     as Foo3
        |entity      Entity      as Foo4
        |database    Database    as Foo5
        |collections Collections as Foo6
        |queue       Queue       as Foo7
        |Foo -> Foo1: To actor
        |Foo -> Foo2: To boundary
        |Foo -> Foo3: To control
        |Foo -> Foo4: To entity
        |Foo -> Foo5: To database
        |Foo -> Foo6: To collections
        |Foo -> Foo7: To queue
        |
        |@enduml""".stripMargin

    val encoded =
      SequenceDiagram(
        participants = ListSet(
          Participant("Participant").withName("Foo"),
          Participant("Actor", Participant.Shape.Actor).withName("Foo1"),
          Participant("Boundary", Participant.Shape.Boundary).withName("Foo2"),
          Participant("Control", Participant.Shape.Control).withName("Foo3"),
          Participant("Entity", Participant.Shape.Entity).withName("Foo4"),
          Participant("Database", Participant.Shape.Database).withName("Foo5"),
          Participant("Collections", Participant.Shape.Collections).withName("Foo6"),
          Participant("Queue", Participant.Shape.Queue).withName("Foo7")
        ),
        messages = List(
          Message("Foo", "Foo1").withText("To actor"),
          Message("Foo", "Foo2").withText("To boundary"),
          Message("Foo", "Foo3").withText("To control"),
          Message("Foo", "Foo4").withText("To entity"),
          Message("Foo", "Foo5").withText("To database"),
          Message("Foo", "Foo6").withText("To collections"),
          Message("Foo", "Foo7").withText("To queue")
        )
      )
        .encode
        .mkString_("\n")

    expect.eql(expected, encoded)
