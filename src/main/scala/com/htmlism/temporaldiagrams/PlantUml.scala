package com.htmlism.temporaldiagrams

object PlantUml {
  private val spotlightStyle =
    """
      |skinparam component {
      |  fontStyle bold
      |  fontColor #AAA
      |  backgroundColor white
      |  borderColor #AAA
      |  borderThickness 2
      |}
      |
      |skinparam queue {
      |  fontStyle bold
      |  fontColor #AAA
      |  backgroundColor white
      |  borderColor #AAA
      |  borderThickness 2
      |}
      |
      |skinparam database {
      |  fontStyle bold
      |  fontColor #AAA
      |  backgroundColor white
      |  borderColor #AAA
      |  borderThickness 2
      |}
      |
      |skinparam component<< Service >> {
      |  fontStyle bold
      |  fontColor white
      |  backgroundColor #586ba4
      |  borderColor #223336
      |  borderThickness 2
      |}""".stripMargin

  implicit val dialect: Dialect[PlantUml] =
    new Dialect[PlantUml] {
      def consume(xs: List[PlantUml], hasHighlights: Boolean): String =
        ("@startuml" :: spotlightStyle :: xs
          .map(consumeOne) ::: List("@enduml"))
          .mkString("\n\n")
    }

  private def consumeOne(x: PlantUml) =
    x match {
      case Component(name, tag) =>
        s"component $name" + tag.fold("")(s => s" << $s >>")

      case Link(src, dest) =>
        s"$src --> $dest"

      case Queue(name, tag) =>
        s"queue $name" + tag.fold("")(s => s" << $s >>")

      case Database(name, tag) =>
        s"database $name" + tag.fold("")(s => s" << $s >>")
    }

  case class Component(name: String, tag: Option[String]) extends PlantUml

  case class Link(src: String, dest: String) extends PlantUml

  case class Queue(name: String, tag: Option[String]) extends PlantUml

  case class Database(name: String, tag: Option[String]) extends PlantUml
}

sealed trait PlantUml
