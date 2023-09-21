package com.htmlism.temporaldiagrams.plantuml

import scala.util.chaining._

import cats.Order
import cats.data._
import cats.syntax.all._

import com.htmlism.temporaldiagrams.v2._

sealed trait PlantUml

object PlantUml {
  implicit val plantUmlOrdering: Order[PlantUml] =
    Order.by { p =>
      val rank =
        p match {
          case Component(name, _, oStereotype) =>
            1 -> name -> oStereotype.toString

          case Queue(name, _, oStereotype) =>
            2 -> name -> oStereotype.toString

          case Database(name, _, oStereotype) =>
            3 -> name -> oStereotype.toString

          case Arrow(src, dest) =>
            10 -> src -> dest

          case SkinParamGroup(base, _, oStereotype) =>
            0 -> base -> oStereotype.toString
        }

      rank
    }

  implicit def nelEncoder[A](implicit A: DiagramEncoder[A]): DiagramEncoder[NonEmptyList[A]] =
    (xs: NonEmptyList[A]) =>
      A.encode(xs.head)
        .appendList(xs.tail.flatMap(x => "" :: A.encode(x).toList))

  implicit def nelHighlightEncoder[A](implicit
      enc: HighlightEncoder[PlantUml, A]
  ): HighlightEncoder[NonEmptyList[PlantUml], A] =
    new HighlightEncoder[NonEmptyList[PlantUml], A] {
      def encode(x: A): NonEmptyList[PlantUml] =
        NonEmptyList.one(enc.encode(x))

      def encodeWithHighlights(x: A, highlighted: Boolean): NonEmptyList[PlantUml] =
        NonEmptyList.one(enc.encodeWithHighlights(x, highlighted))
    }

  implicit val DiagramEncoder: DiagramEncoder[PlantUml] = {
    case Component(name, oAlias, oStereotype) =>
      s"component $name"
        .applySome(oAlias)((s, a) => s + s" as $a")
        .applySome(oStereotype)((s, st) => s + s" << $st >>")
        .pipe(NonEmptyList.one)

    case Queue(name, oAlias, oStereotype) =>
      s"queue $name"
        .applySome(oAlias)((s, a) => s + s" as $a")
        .applySome(oStereotype)((s, st) => s + s" << $st >>")
        .pipe(NonEmptyList.one)

    case Database(name, oAlias, oStereotype) =>
      s"database $name"
        .applySome(oAlias)((s, a) => s + s" as $a")
        .applySome(oStereotype)((s, st) => s + s" << $st >>")
        .pipe(NonEmptyList.one)

    case Arrow(src, dest) =>
      NonEmptyList.one(s"$src --> $dest")

    case SkinParamGroup(base, parameters, oStereotype) =>
      parameters
        .map { case SkinParamGroup.Parameter(key, value) =>
          s"  $key $value"
        }
        .prepended {
          val stereotype =
            oStereotype.fold("")("<< " + _ + " >>")

          s"skinparam $base$stereotype {"
        }
        .pipe(NonEmptyList.one("}").prependList)
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
    * @param stereotype
    *   Optional. A tag to share styling among components of the same stereotype
    */
  case class Component(name: String, alias: Option[String], stereotype: Option[String]) extends PlantUml

  case class Queue(name: String, alias: Option[String], stereotype: Option[String]) extends PlantUml

  case class Database(name: String, alias: Option[String], stereotype: Option[String]) extends PlantUml

  /**
    * A directed line from the source to the destination
    *
    * In terms of "gravity", the source is always first. In top-down diagrams, the source is on the top and the
    * destination is on the bottom. In left-to-right diagrams, the source is on the left and the destination is on the
    * right.
    */
  case class Arrow(source: String, destination: String) extends PlantUml

  case class SkinParamGroup(base: String, parameters: List[SkinParamGroup.Parameter], stereotype: Option[String])
      extends PlantUml {
    def and(key: String, value: String): SkinParamGroup =
      this.copy(parameters = parameters.appended(SkinParamGroup.Parameter(key, value)))
  }

  object SkinParamGroup {
    case class Parameter(name: String, value: String)

    def apply(base: String): SkinParamGroup =
      SkinParamGroup(base, Nil, None)

    def apply(base: String, stereotype: String): SkinParamGroup =
      SkinParamGroup(base, Nil, stereotype.some)
  }

  private def asDocument(xs: NonEmptyList[String]) =
    NonEmptyList.of("@startuml", "") :::
      xs :::
      NonEmptyList.of("", "@enduml")
}
