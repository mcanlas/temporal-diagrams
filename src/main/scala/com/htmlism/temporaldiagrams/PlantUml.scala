package com.htmlism.temporaldiagrams

import cats.syntax.all._

object PlantUml {
  implicit val dialect: Dialect[PlantUml] =
    new Dialect[PlantUml] {
      def consume(xs: List[PlantUml], injectedStyle: String): String = {
        val entities =
          xs.collect { case x: Entity => x }

        val relationships =
          xs.collect { case x: Link => x }

        ("@startuml" :: injectedStyle :: (entities ::: relationships)
          .map(consumeOne) ::: List("@enduml"))
          .mkString("\n\n")
      }
    }

  sealed trait Entity extends PlantUml

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

      case Database(name, title, tag, xs) =>
        val header =
          oneThing("database", name, title, tag)

        xs match {
          case Nil =>
            header

          case _ =>
            val open =
              header + " {"

            val close =
              "}"

            (open :: xs.map(consumeOne) ::: List(close))
              .mkString("\n")
        }

      case Package(title, xs) =>
        (s"package \"$title\" {" :: xs.map(consumeOne).mkString("\n") :: List("}")).mkString("\n")

      case Actor(name, title, tag) =>
        oneThing("actor", name, title, tag)

      case UseCase(name, title, tag) =>
        oneThing("usecase", name, title, tag)
    }

  case class Component(name: String, title: Option[String], tag: Option[String]) extends Entity {
    def of(stereotype: String): Component =
      this.copy(tag = stereotype.some)
  }

  object Component {
    def apply(name: String): Component =
      Component(name, None, None)
  }

  case class Link(src: String, dest: String) extends PlantUml

  case class Queue(name: String, title: Option[String], tag: Option[String]) extends Entity

  case class Database(name: String, title: Option[String], tag: Option[String], xs: List[PlantUml]) extends Entity

  case class Package(title: String, xs: List[PlantUml]) extends Entity

  case class Actor(name: String, title: Option[String], tag: Option[String]) extends Entity

  case class UseCase(name: String, title: Option[String], tag: Option[String]) extends Entity
}

sealed trait PlantUml
