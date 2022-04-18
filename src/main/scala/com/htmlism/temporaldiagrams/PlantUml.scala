package com.htmlism.temporaldiagrams

object PlantUml {
  implicit val dialect: Dialect[PlantUml] =
    new Dialect[PlantUml] {
      def consume(xs: List[PlantUml], injectedStyle: String): String =
        ("@startuml" :: injectedStyle :: xs
          .map(consumeOne) ::: List("@enduml"))
          .mkString("\n\n")
    }

  private def consumeOne(x: PlantUml) =
    x match {
      case Component(name, title, tag) =>
        val maybeTitle =
          title.fold(List.empty[String])(s => List(s"\"$s\"", "as"))

        val maybeTag =
          tag.fold(List.empty[String])(s => List(s"<< $s >>"))

        ("component" :: maybeTitle ::: name :: maybeTag)
          .mkString(" ")

      case Link(src, dest) =>
        s"$src --> $dest"

      case Queue(name, tag) =>
        s"queue $name" + tag.fold("")(s => s" << $s >>")

      case Database(name, tag) =>
        s"database $name" + tag.fold("")(s => s" << $s >>")
    }

  case class Component(name: String, title: Option[String], tag: Option[String]) extends PlantUml

  case class Link(src: String, dest: String) extends PlantUml

  case class Queue(name: String, tag: Option[String]) extends PlantUml

  case class Database(name: String, tag: Option[String]) extends PlantUml
}

sealed trait PlantUml
