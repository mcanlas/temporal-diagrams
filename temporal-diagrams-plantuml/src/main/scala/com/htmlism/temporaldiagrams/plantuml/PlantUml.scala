package com.htmlism.temporaldiagrams.plantuml

import scala.util.chaining._

import cats.data._

trait PlantUml

object PlantUml {
  implicit def nelEncoder[A](implicit A: PlantUmlEncoder[A]): PlantUmlEncoder[NonEmptyList[A]] =
    new PlantUmlEncoder[NonEmptyList[A]] {
      def encode(x: NonEmptyList[A]): NonEmptyList[String] =
        x.flatMap(A.encode)
    }

  def render[A](x: A)(implicit A: PlantUmlEncoder[A]): NonEmptyList[String] =
    A.encode(x).pipe(asDocument)

  private def asDocument(xs: NonEmptyList[String]) =
    xs
      .prepend("@startuml")
      .append("@enduml")
}
