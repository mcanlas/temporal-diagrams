package com.htmlism.temporaldiagrams

object PlantUml {
  implicit val dialect: Dialect[PlantUml] =
    new Dialect[PlantUml] {
      def joiner: String =
        "\n\n"
    }
}

class PlantUml
