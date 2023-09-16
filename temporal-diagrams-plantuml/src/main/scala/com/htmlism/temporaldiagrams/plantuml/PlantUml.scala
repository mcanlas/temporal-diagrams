package com.htmlism.temporaldiagrams.plantuml

import scala.util.chaining._

import cats.data._

import com.htmlism.temporaldiagrams.v2.DiagramEncoder

sealed trait PlantUml

object PlantUml {
  implicit def nelEncoder[A](implicit A: DiagramEncoder[A]): DiagramEncoder[NonEmptyList[A]] =
    (xs: NonEmptyList[A]) =>
      A.encode(xs.head)
        .appendList(xs.tail.flatMap(x => "" :: A.encode(x).toList))

  implicit val DiagramEncoder: DiagramEncoder[PlantUml] = {
    case Component(name, alias) =>
      s"component $name"
        .applyWhen(alias)((s, a) => s + s" as $a")
        .pipe(NonEmptyList.one)

    case Arrow(src, dest) =>
      NonEmptyList.one(s"$src --> $dest")
  }

  def render[A](x: A)(implicit A: DiagramEncoder[A]): NonEmptyList[String] =
    A.encode(x).pipe(asDocument)

  /**
    * A building block in component diagrams
    *
    * @param name
    *   This text is visible in the diagram
    * @param alias
    *   Optional. If provided, arrows can refer to this component by this alias. If not provided, the alias will be the
    *   value of `name`
    */
  case class Component(name: String, alias: Option[String]) extends PlantUml

  /**
    * A directed line from the source to the destination
    *
    * In terms of "gravity", the source is always first. In top-down diagrams, the source is on the top and the
    * destination is on the bottom. In left-to-right diagrams, the source is on the left and the destination is on the
    * right.
    */
  case class Arrow(source: String, destination: String) extends PlantUml

  private def asDocument(xs: NonEmptyList[String]) =
    NonEmptyList.of("@startuml", "") :::
      xs :::
      NonEmptyList.of("", "@enduml")
}
