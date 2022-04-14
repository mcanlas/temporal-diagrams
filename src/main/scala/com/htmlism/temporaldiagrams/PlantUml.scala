package com.htmlism.temporaldiagrams

object PlantUml {
  implicit val dialect: Dialect[PlantUml] =
    new Dialect[PlantUml] {
      def joiner: String =
        "\n\n"
    }
}

sealed trait PlantUml

case class Component(name: String) extends PlantUml

case class Link(src: String, dest: String) extends PlantUml

case class Queue(name: String) extends PlantUml
