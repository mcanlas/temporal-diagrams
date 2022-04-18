package com.htmlism.temporaldiagrams

object PlantUml {
  implicit val dialect: Dialect[PlantUml] =
    new Dialect[PlantUml] {
      def consume(xs: List[PlantUml]): String =
        xs
          .map(consumeOne)
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
