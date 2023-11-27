package com.htmlism.temporaldiagrams.plantuml

import scala.util.chaining.*

import cats.*
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

          case _: Link =>
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

  given DiagramEncoder[PlantUml] with
    def encode(x: PlantUml): Chain[String] =
      x match
        case Package(name, xs) =>
          xs
            .pipe(Chain.fromSeq)
            .pipe(summon[DiagramEncoder[Chain[PlantUml]]].encode)
            .map { s =>
              if s.isEmpty then ""
              else "  " + s
            }
            .prepend(s"package ${safeQuote(name)} {")
            .append("}")

        case Component(name, oAlias, oStereotype) =>
          Chain:
            s"component ${safeQuote(name)}"
              .applySome(oAlias)((s, a) => s + s" as ${id(a)}")
              .applySome(oStereotype)((s, st) => s + s" << $st >>")

        case Queue(name, oAlias, oStereotype) =>
          Chain:
            s"queue ${safeQuote(name)}"
              .applySome(oAlias)((s, a) => s + s" as ${id(a)}")
              .applySome(oStereotype)((s, st) => s + s" << $st >>")

        case Database(name, oAlias, oStereotype, xs) =>
          val slug =
            s"database ${safeQuote(name)}"
              .applySome(oAlias)((s, a) => s + s" as ${id(a)}")
              .applySome(oStereotype)((s, st) => s + s" << $st >>")

          if xs.isEmpty then
            Chain:
              slug
          else
            xs
              .pipe(Chain.fromSeq)
              .pipe(summon[DiagramEncoder[Chain[PlantUml]]].encode)
              .map { s =>
                if s.isEmpty then ""
                else "  " + s
              }
              .prepend(s"$slug {")
              .append("}")

        case Interface(name, oAlias) =>
          Chain:
            s"interface ${safeQuote(name)}"
              .applySome(oAlias)((s, a) => s + s" as ${id(a)}")

        case x: Link =>
          DiagramEncoder[PlantUml.Link].encode(x)

        case x: Directive =>
          DiagramEncoder[PlantUml.Directive].encode(x)

  // TODO test
  case class ComponentDiagram(parameters: Set[PlantUml.Directive], entities: Set[PlantUml.Entity], links: Set[Link])

  object ComponentDiagram:
    given Monoid[ComponentDiagram] =
      new Monoid[ComponentDiagram]:
        def empty: ComponentDiagram =
          ComponentDiagram(Set.empty, Set.empty, Set.empty)

        def combine(x: ComponentDiagram, y: ComponentDiagram): ComponentDiagram =
          ComponentDiagram(
            x.parameters ++ y.parameters,
            x.entities ++ y.entities,
            x.links ++ y.links
          )

    /**
      * A sequence-builder-style factory method for constructing [[ComponentDiagram]] where the user doesn't need to
      * keep track of the different diagram parts
      */
    def apply(xs: PlantUml*): ComponentDiagram =
      ComponentDiagram(
        xs.collect { case x: PlantUml.Directive => x }.toSet,
        xs.collect { case x: PlantUml.Entity => x }.toSet,
        xs.collect { case x: PlantUml.Link => x }.toSet
      )

  // TODO test
  private def safeQuote(s: String) =
    if s.contains("-") || s.contains(" ") then s"\"$s\""
    else s

  private def id(s: String) =
    s
      .replace("-", "_")
      .replace(" ", "_")

  // TODO test this
  def render(xs: Chain[PlantUml]): Chain[String] =
    xs
      .distinct
      .sorted
      .pipe(DiagramEncoder[Chain[PlantUml]].encode)
      .pipe(asDocument)

  // TODO test
  def renderBasket(x: PlantUml.ComponentDiagram): Chain[String] =
    Chain(x.parameters, x.entities, x.links)
      .map(_.toList)
      .map(Chain.fromSeq)
      .map(DiagramEncoder[Chain[PlantUml]].encode)
      .flatMap(_.sorted)

  /**
    * Something that isn't a link and isn't an entity
    */
  sealed trait Directive extends PlantUml

  object Directive:
    given DiagramEncoder[Directive] with
      def encode(x: Directive): Chain[String] =
        x match
          case LeftToRightDirection =>
            Chain:
              "left to right direction"

          case SkinParam(key, value) =>
            Chain:
              s"skinparam $key $value"

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

  case object LeftToRightDirection extends Directive

  // something that can be nested in a package; or is global in scope, like an arrow
  sealed trait Entity extends PlantUml

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
  case class Component(name: String, alias: Option[String], stereotype: Option[String]) extends Entity

  case class Queue(name: String, alias: Option[String], stereotype: Option[String]) extends Entity

  case class Database(name: String, alias: Option[String], stereotype: Option[String], xs: List[Entity]) extends Entity

  // TODO
  case class Interface(name: String, alias: Option[String]) extends Entity

  /**
    * A link from the source to the destination
    *
    * In terms of "gravity", the source is always first. In top-down diagrams, the source is on the top and the
    * destination is on the bottom. In left-to-right diagrams, the source is on the left and the destination is on the
    * right.
    *
    * @param source
    *   The base side of the link, where it originates
    * @param destination
    *   The tip side of the link, where it stops
    * @param text
    *   Optional text written along the link
    */
  // TODO test
  case class Link(source: String, destination: String, length: Int, direction: Link.Direction, text: Option[String])
      extends PlantUml

  object Link:
    enum Direction:
      case Forwards
      case Backwards
      case Bidirectional

    given DiagramEncoder[Link] with
      def encode(x: Link): Chain[String] =
        x match
          case Link(src, dest, length, dir, oText) =>
            val body =
              "-" * length

            val (leftHead, rightHead) =
              dir match
                case Link.Direction.Forwards =>
                  "" -> ">"

                case Link.Direction.Backwards =>
                  "<" -> ""

                case Link.Direction.Bidirectional =>
                  "<" -> ">"

            Chain:
              s"${safeQuote(src)} $leftHead$body$rightHead ${safeQuote(dest)}"
                .applySome(oText)((s, t) => s"$s : $t")

  // TODO test
  case class SkinParam(key: String, value: String) extends Directive

  case class SkinParamGroup(base: String, parameters: List[SkinParamGroup.Parameter], stereotype: Option[String])
      extends Directive:
    def and(key: String, value: String): SkinParamGroup =
      this.copy(parameters = parameters.appended(SkinParamGroup.Parameter(key, value)))

  object SkinParamGroup:
    case class Parameter(name: String, value: String)

    def apply(base: String): SkinParamGroup =
      SkinParamGroup(base, Nil, None)

    def apply(base: String, stereotype: String): SkinParamGroup =
      SkinParamGroup(base, Nil, stereotype.some)

  case class Package(name: String, xs: List[Entity]) extends Entity

  object Package:
    def apply(name: String, xs: Entity*): Package =
      Package(name, xs.toList)

  private def asDocument(xs: Chain[String]) =
    Chain("@startuml", "") ++
      xs ++
      Chain("", "@enduml")
