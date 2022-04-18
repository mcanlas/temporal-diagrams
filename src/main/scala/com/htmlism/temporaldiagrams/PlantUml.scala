package com.htmlism.temporaldiagrams

object PlantUml {
  implicit val dialect: Dialect[PlantUml] =
    new Dialect[PlantUml] {
      def consume(xs: List[PlantUml], injectedStyle: String): String =
        ("@startuml" :: injectedStyle :: xs
          .map(consumeOne) ::: List("@enduml"))
          .mkString("\n\n")
    }

  private def oneThing(thing: String, name: String, title: Option[String], tag: Option[String]) = {
    val maybeTitle =
      title.fold(List.empty[String])(s => List(s"\"$s\"", "as"))

    val maybeTag =
      tag.fold(List.empty[String])(s => List(s"<< $s >>"))

    (thing :: maybeTitle ::: name :: maybeTag)
      .mkString(" ")
  }

  private def consumeOne(x: PlantUml): String =
    x match {
      case Component(name, title, tag) =>
        oneThing("component", name, title, tag)

      case Link(src, dest) =>
        s"$src --> $dest"

      case Queue(name, title, tag) =>
        oneThing("queue", name, title, tag)

      case Database(name, title, tag) =>
        oneThing("database", name, title, tag)

      case Package(title, xs) =>
        (s"package \"$title\" {" :: xs.map(consumeOne).mkString("\n") :: List("}")).mkString("\n")
    }

  case class Component(name: String, title: Option[String], tag: Option[String]) extends PlantUml

  case class Link(src: String, dest: String) extends PlantUml

  case class Queue(name: String, title: Option[String], tag: Option[String]) extends PlantUml

  case class Database(name: String, title: Option[String], tag: Option[String]) extends PlantUml

  case class Package(title: String, xs: List[PlantUml]) extends PlantUml
}

sealed trait PlantUml
