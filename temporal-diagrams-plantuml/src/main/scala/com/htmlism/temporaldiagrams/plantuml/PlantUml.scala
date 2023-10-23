package com.htmlism.temporaldiagrams.plantuml

import scala.util.chaining.*

import cats.Order
import cats.data.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.v2.*

sealed trait PlantUml

object PlantUml:
  implicit val plantUmlOrdering: Order[PlantUml] =
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

  implicit def necEncoder[A](using A: DiagramEncoder[A]): DiagramEncoder[NonEmptyChain[A]] =
    (xs: NonEmptyChain[A]) =>
      A.encode(xs.head)
        .appendChain(xs.tail.flatMap(x => "" +: A.encode(x).toChain))

  implicit def necHighlightEncoder[A](using
      enc: HighlightEncoder[PlantUml, A]
  ): HighlightEncoder[NonEmptyChain[PlantUml], A] =
    new HighlightEncoder[NonEmptyChain[PlantUml], A]:
      def encode(x: A): NonEmptyChain[PlantUml] =
        NonEmptyChain.one(enc.encode(x))

      def encodeWithHighlights(x: A, highlighted: Boolean): NonEmptyChain[PlantUml] =
        NonEmptyChain.one(enc.encodeWithHighlights(x, highlighted))

  implicit val plantUmlDiagramEncoder: DiagramEncoder[PlantUml] =
    case LeftToRightDirection =>
      "left to right direction"
        .pipe(NonEmptyChain.one)

    case Component(name, oAlias, oStereotype) =>
      s"component $name"
        .applySome(oAlias)((s, a) => s + s" as $a")
        .applySome(oStereotype)((s, st) => s + s" << $st >>")
        .pipe(NonEmptyChain.one)

    case Queue(name, oAlias, oStereotype) =>
      s"queue $name"
        .applySome(oAlias)((s, a) => s + s" as $a")
        .applySome(oStereotype)((s, st) => s + s" << $st >>")
        .pipe(NonEmptyChain.one)

    case Database(name, oAlias, oStereotype) =>
      s"database $name"
        .applySome(oAlias)((s, a) => s + s" as $a")
        .applySome(oStereotype)((s, st) => s + s" << $st >>")
        .pipe(NonEmptyChain.one)

    case Arrow(src, dest, oText) =>
      s"$src --> $dest"
        .applySome(oText)((s, t) => s"$s : $t")
        .pipe(NonEmptyChain.one)

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
        .pipe(NonEmptyChain.one("}").prependChain)

  // TODO test this
  def render(xs: NonEmptyChain[PlantUml]): NonEmptyChain[String] =
    xs
      .distinct
      .sorted
      .pipe(DiagramEncoder[NonEmptyChain[PlantUml]].encode)
      .pipe(asDocument)

  def renderHorizontally(xs: NonEmptyChain[PlantUml]): NonEmptyChain[String] =
    xs
      .distinct
      .sorted
      .prepend(LeftToRightDirection)
      .pipe(DiagramEncoder[NonEmptyChain[PlantUml]].encode)
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

  private def asDocument(xs: NonEmptyChain[String]) =
    NonEmptyChain.of("@startuml", "") ++
      xs ++
      NonEmptyChain.of("", "@enduml")
