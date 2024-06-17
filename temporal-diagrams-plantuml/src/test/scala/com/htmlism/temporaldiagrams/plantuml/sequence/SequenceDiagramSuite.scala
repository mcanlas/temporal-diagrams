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
          Participant.Basic("Alice"),
          Participant.Basic("Bob")
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
          Participant.Basic("Participant").withAlias("Foo"),
          Participant.Basic("Actor", Participant.Shape.Actor).withAlias("Foo1"),
          Participant.Basic("Boundary", Participant.Shape.Boundary).withAlias("Foo2"),
          Participant.Basic("Control", Participant.Shape.Control).withAlias("Foo3"),
          Participant.Basic("Entity", Participant.Shape.Entity).withAlias("Foo4"),
          Participant.Basic("Database", Participant.Shape.Database).withAlias("Foo5"),
          Participant.Basic("Collections", Participant.Shape.Collections).withAlias("Foo6"),
          Participant.Basic("Queue", Participant.Shape.Queue).withAlias("Foo7")
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

  test("PlantUML.com participant color example"):
    val expected =
      """@startuml
        |
        |actor       Bob                               #red
        |participant Alice
        |participant "I have a really\nlong name" as L #99FF99
        |Alice -> Bob: Authentication Request
        |Bob -> Alice: Authentication Response
        |Bob -> L: Log transaction
        |
        |@enduml""".stripMargin

    val encoded =
      SequenceDiagram(
        participants = ListSet(
          Participant
            .Basic("Bob", Participant.Shape.Actor)
            .withColor("#red"),
          Participant.Basic("Alice"),
          Participant
            .Basic("I have a really\\nlong name")
            .withAlias("L")
            .withColor("#99FF99")
        ),
        messages = List(
          Message("Alice", "Bob").withText("Authentication Request"),
          Message("Bob", "Alice").withText("Authentication Response"),
          Message("Bob", "L").withText("Log transaction")
        )
      )
        .encode
        .mkString_("\n")

    expect.eql(expected, encoded)

  test("PlantUML.com order example"):
    val expected =
      """@startuml
        |
        |participant Last   order 30
        |participant Middle order 20
        |participant First  order 10
        |
        |@enduml""".stripMargin

    val encoded =
      SequenceDiagram(
        participants = ListSet(
          Participant
            .Basic("Last")
            .withOrder(30),
          Participant
            .Basic("Middle")
            .withOrder(20),
          Participant
            .Basic("First")
            .withOrder(10)
        )
      )
        .encode
        .mkString_("\n")

    expect.eql(expected, encoded)

  test("PlantUML.com multiline example"):
    val expected =
      """@startuml
        |
        |participant Participant [
        |  =Title
        |  ----
        |  ""SubTitle""
        |]
        |participant Bob
        |Participant -> Bob
        |
        |@enduml""".stripMargin

    val encoded =
      SequenceDiagram(
        participants = ListSet(
          Participant.MultiLine("Participant", List("=Title", "----", "\"\"SubTitle\"\"")),
          Participant.Basic("Bob")
        ),
        messages = List(
          Message("Participant", "Bob")
        )
      )
        .encode
        .mkString_("\n")

    expect.eql(expected, encoded)

  test("Multiline participant with order"):
    val expected =
      """@startuml
        |
        |participant High order 50 [
        |  =Title
        |  ----
        |  ""SubTitle""
        |]
        |participant Low order 10 [
        |  =Title
        |  ----
        |  ""SubTitle""
        |]
        |
        |@enduml""".stripMargin

    val encoded =
      SequenceDiagram(
        participants = ListSet(
          Participant.MultiLine("High", List("=Title", "----", "\"\"SubTitle\"\""), 50.some),
          Participant.MultiLine("Low", List("=Title", "----", "\"\"SubTitle\"\""), 10.some)
        )
      )
        .encode
        .mkString_("\n")

    expect.eql(expected, encoded)

  // TODO test message escaping
  test("Quoting"):
    val expected =
      """@startuml
        |
        |participant "Bob ()"
        |
        |@enduml""".stripMargin

    val encoded =
      SequenceDiagram(
        participants = ListSet(
          Participant.Basic("Bob ()")
        )
      )
        .encode
        .mkString_("\n")

    expect.eql(expected, encoded)
