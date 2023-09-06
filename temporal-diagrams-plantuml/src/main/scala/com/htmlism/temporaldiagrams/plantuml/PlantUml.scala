package com.htmlism.temporaldiagrams.plantuml

import scala.util.chaining._

import cats.data._

trait PlantUml

object PlantUml {
  implicit def nelEncoder[A](implicit A: PlantUmlEncoder[A]): PlantUmlEncoder[NonEmptyList[A]] =
    new PlantUmlEncoder[NonEmptyList[A]] {
      def encode(xs: NonEmptyList[A]): NonEmptyList[String] =
        A.encode(xs.head)
          .appendList(xs.tail.flatMap(x => "" :: A.encode(x).toList))
    }

  def render[A](x: A)(implicit A: PlantUmlEncoder[A]): NonEmptyList[String] =
    A.encode(x).pipe(asDocument)

  private def asDocument(xs: NonEmptyList[String]) =
    xs
      .prepend("")
      .prepend("@startuml")
      .append("")
      .append("@enduml")
}
