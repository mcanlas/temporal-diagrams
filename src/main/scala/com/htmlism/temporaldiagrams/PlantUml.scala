package com.htmlism.temporaldiagrams

object PlantUml {
  implicit val dialect: Dialect[PlantUml] =
    new Dialect[PlantUml] {
      def joiner: String =
        "\n\n"

      def consume(xs: List[PlantUml]): String =
        xs
          .map(consumeOne)
          .mkString("\n\n")
    }

  private def consumeOne(x: PlantUml) =
    x match {
      case Component(name) =>
        s"component $name"

      case Link(src, dest) =>
        s"$src --> $dest"

      case Queue(name) =>
        s"queue $name"
    }
}

sealed trait PlantUml

case class Component(name: String) extends PlantUml

case class Link(src: String, dest: String) extends PlantUml

case class Queue(name: String) extends PlantUml
