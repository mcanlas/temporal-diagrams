package com.htmlism.temporaldiagrams.plantuml

import scala.util.chaining.*

import cats.Order
import cats.data.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.v2.*

sealed trait PlantUml

object PlantUml:
  given Order[PlantUml] =
    Order.by { x =>
      val index =
        x match
          case LeftToRightDirection =>
            0

          case _: SkinParamGroup =>
            1

          case _: Arrow =>
            // if arrows refer to a component that isn't defined, they will implicitly create their own;
            // so allow components to take priority
            10

          case _ =>
            2

      val str =
        summon[DiagramEncoder[PlantUml]]
          .encode(x)

      index -> str
    }

  /**
    * Handles the string encoding of multiple diagram components, based off the encoding of a single element/ADT
    *
    * @tparam A
    *   The target diagram language
    */
  given [A](using A: DiagramEncoder[A]): DiagramEncoder[Chain[A]] =
    (xs: Chain[A]) =>
      xs.uncons match
        case Some(head, tail) =>
          A.encode(head)
            .concat(tail.flatMap(x => "" +: A.encode(x)))

        case None =>
          Chain.empty

  /**
    * If a domain language is 1:1 with PlantUML declarations, this automatically promotes the encoder to be monoidal, a
    * requirement for diagram rendering
    *
    * @tparam A
    *   The input domain language
    */
  given [A](using
      enc: HighlightEncoder[PlantUml, A]
  ): HighlightEncoder[Chain[PlantUml], A] =
    new HighlightEncoder[Chain[PlantUml], A]:
      def encode(x: A): Chain[PlantUml] =
        Chain(enc.encode(x))

      def encodeWithHighlights(x: A, highlighted: Boolean): Chain[PlantUml] =
        Chain(enc.encodeWithHighlights(x, highlighted))

  given DiagramEncoder[PlantUml] =
    case LeftToRightDirection =>
      "left to right direction"
        .pipe(Chain(_))

    case Component(name, oAlias, oStereotype) =>
      s"component $name"
        .applySome(oAlias)((s, a) => s + s" as $a")
        .applySome(oStereotype)((s, st) => s + s" << $st >>")
        .pipe(Chain(_))

    case Queue(name, oAlias, oStereotype) =>
      s"queue $name"
        .applySome(oAlias)((s, a) => s + s" as $a")
        .applySome(oStereotype)((s, st) => s + s" << $st >>")
        .pipe(Chain(_))

    case Database(name, oAlias, oStereotype) =>
      s"database $name"
        .applySome(oAlias)((s, a) => s + s" as $a")
        .applySome(oStereotype)((s, st) => s + s" << $st >>")
        .pipe(Chain(_))

    case Arrow(src, dest, oText) =>
      s"$src --> $dest"
        .applySome(oText)((s, t) => s"$s : $t")
        .pipe(Chain(_))

    case SkinParamGroup(base, parameters, oStereotype) =>
      parameters
        .map { case SkinParamGroup.Parameter(key, value) =>
          s"  $key $value"
        }
        .prepended:
          val stereotype =
            oStereotype.fold("")("<< " + _ + " >>")

          s"skinparam $base$stereotype {"
        .pipe(Chain.fromSeq)
        .pipe(_ ++ Chain("}"))

  // TODO test this
  def render(xs: Chain[PlantUml]): Chain[String] =
    xs
      .distinct
      .sorted
      .pipe(DiagramEncoder[Chain[PlantUml]].encode)
      .pipe(asDocument)

  def renderHorizontally(xs: Chain[PlantUml]): Chain[String] =
    xs
      .distinct
      .sorted
      .prepend(LeftToRightDirection)
      .pipe(DiagramEncoder[Chain[PlantUml]].encode)
      .pipe(asDocument)

  case object LeftToRightDirection extends PlantUml

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
    *
    * @param source
    *   The base side of the arrow, where it originates
    * @param destination
    *   The tip side of the arrow, where it stops
    * @param text
    *   Optional text written along the arrow
    */
  case class Arrow(source: String, destination: String, text: Option[String]) extends PlantUml

  case class SkinParamGroup(base: String, parameters: List[SkinParamGroup.Parameter], stereotype: Option[String])
      extends PlantUml:
    def and(key: String, value: String): SkinParamGroup =
      this.copy(parameters = parameters.appended(SkinParamGroup.Parameter(key, value)))

  object SkinParamGroup:
    case class Parameter(name: String, value: String)

    def apply(base: String): SkinParamGroup =
      SkinParamGroup(base, Nil, None)

    def apply(base: String, stereotype: String): SkinParamGroup =
      SkinParamGroup(base, Nil, stereotype.some)

  private def asDocument(xs: Chain[String]) =
    Chain("@startuml", "") ++
      xs ++
      Chain("", "@enduml")
