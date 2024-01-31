package com.htmlism.temporaldiagrams

import cats.syntax.all.*

import com.htmlism.temporaldiagrams.syntax.*

object PlantUml:
  def render(xs: List[PlantUml]): String =
    renderWithDirection(None, xs)

  def renderHorizontally(xs: List[PlantUml]): String =
    renderWithDirection("left to right direction".some, xs)

  private def renderWithDirection(direction: Option[String], xs: List[PlantUml]) =
    val singletons =
      xs.collect { case x: Singleton => x }

    val skins =
      xs.collect { case x: SkinParam => x }.distinct

    val entities =
      xs.collect { case x: Entity => x }.distinct

    val relationships =
      xs.collect { case x: Link => x }

    ("@startuml" :: direction.toList ::: (singletons ::: skins ::: entities ::: relationships)
      .flatMap(consumeOne) ::: List("@enduml"))
      .mkString("\n\n")

  sealed trait Singleton extends PlantUml

  case class Title(s: String) extends Singleton

  case class Legend(xs: List[String]) extends Singleton

  sealed trait SkinParam extends PlantUml

  object SkinParam:
    case class Single(key: String, value: String) extends SkinParam

    case class Bundle(entity: String, stereotype: Option[String], xs: List[Single]) extends SkinParam:
      def and(key: String, value: String): Bundle =
        this.copy(xs = xs appended Single(key, value))

    def build(entity: String): Bundle =
      Bundle(entity, None, Nil)

    def build(entity: String, stereotype: String): Bundle =
      Bundle(entity, stereotype.some, Nil)

  sealed trait Entity extends PlantUml

  private def oneThing(thing: String, name: String, title: Option[String], tag: Option[String]) =
    val maybeTitle =
      title.fold(List.empty[String])(s => List(s"\"$s\"", "as"))

    val maybeTag =
      tag.fold(List.empty[String])(s => List(s"<< $s >>"))

    (thing :: maybeTitle ::: name :: maybeTag)
      .mkString(" ")

  private def consumeOne(x: PlantUml): List[String] =
    x match
      case Title(str) =>
        s"title $str".list

      case Legend(xs) =>
        ("legend" :: xs appended "end legend").mkString("\n").list

      case Interface(name, title) =>
        title
          .fold(s"interface $name")(t => s"interface \"$t\" as $name")
          .list

      case Component(name, title, tag) =>
        oneThing("component", name, title, tag).list

      case Link(src, dest, length, weight, direction, oColor, oComment, withRank, oStereotype) =>
        val (segment, style) =
          weight match
            case Link.Weight.Solid =>
              "-" -> Nil

            case Link.Weight.Dotted =>
              "." -> Nil

            case Link.Weight.Bold =>
              "-" -> List("bold")

        val styles =
          style ++ oColor.map(s => "#" + s).toList ++ (if withRank then Nil else List("norank"))

        val stylesStr =
          if styles.isEmpty then ""
          else "[" + styles.mkString(",") + "]"

        val (left, right) =
          direction match
            case Link.Direction.Forwards =>
              "" -> ">"

            case Link.Direction.Backwards =>
              "<" -> ""

            case Link.Direction.Bidirectional =>
              "<" -> ">"

        val remainingLength =
          segment * (length - 1)

        val srcArrowDest =
          s"$src $left$segment$stylesStr$remainingLength$right $dest"

        val comment =
          oComment.map(" : " + _).toList

        val stereotype =
          oStereotype.map(s => s" << $s >>").toList

        (srcArrowDest :: stereotype ::: comment)
          .mkString
          .list

      case Database(name, title, tag, xs) =>
        val header =
          oneThing("database", name, title, tag)

        xs match
          case Nil =>
            header.list

          case _ =>
            val open =
              header + " {"

            val close =
              "}"

            open :: xs.flatMap(consumeOne) appended close

      case Package(title, xs) =>
        s"package \"$title\" {" :: xs.flatMap(consumeOne).map("  " + _).mkString("\n\n").list appended "}"

      case SkinParam.Single(k, v) =>
        s"skinparam $k $v".list

      case SkinParam.Bundle(entity, oStereotype, xs) =>
        val stereotype =
          oStereotype.fold("")(s => s"<< $s >>")

        (s"skinparam $entity$stereotype {" :: xs
          .map { case SkinParam.Single(k, v) => s"  $k $v" } appended "}")
          .mkString("\n")
          .list

  case class Interface(name: String, title: Option[String]) extends Entity

  case class Component(name: String, title: Option[String], tag: Option[String]) extends Entity:
    def of(stereotype: String): Component =
      this.copy(tag = stereotype.some)

  object Component:
    def apply(name: String): Component =
      Component(name, None, None)

  case class Link(
      src: String,
      dest: String,
      length: Int,
      weight: Link.Weight,
      direction: Link.Direction,
      color: Option[String],
      comment: Option[String],
      influencesRank: Boolean,
      stereotype: Option[String]
  ) extends PlantUml

  object Link:
    sealed trait Weight

    object Weight:
      case object Solid  extends Weight
      case object Dotted extends Weight
      case object Bold   extends Weight

    sealed trait Direction

    object Direction:
      case object Forwards      extends Direction
      case object Backwards     extends Direction
      case object Bidirectional extends Direction

    def apply(src: String, dest: String): Link =
      Link(src, dest, 2, Weight.Solid, Direction.Forwards, None, None, influencesRank = true, None)

  case class Database(name: String, title: Option[String], tag: Option[String], xs: List[PlantUml]) extends Entity

  case class Package(title: String, xs: List[PlantUml]) extends Entity

sealed trait PlantUml
