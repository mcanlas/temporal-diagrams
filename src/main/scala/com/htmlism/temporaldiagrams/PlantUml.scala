package com.htmlism.temporaldiagrams

import cats.syntax.all._

import com.htmlism.temporaldiagrams.syntax._

object PlantUml {
  def render(injectedStyle: String)(xs: List[PlantUml]): String =
    renderWithDirection(injectedStyle, None, xs)

  def renderHorizontally(injectedStyle: String)(xs: List[PlantUml]): String =
    renderWithDirection(injectedStyle, "left to right direction".some, xs)

  private def renderWithDirection(injectedStyle: String, direction: Option[String], xs: List[PlantUml]) = {
    val skins =
      xs.collect { case x: SkinParam => x }.distinct

    val entities =
      xs.collect { case x: Entity => x }.distinct

    val relationships =
      xs.collect { case x: Link => x }

    ("@startuml" :: direction.toList ::: injectedStyle :: (skins ::: entities ::: relationships)
      .flatMap(consumeOne) ::: List("@enduml"))
      .mkString("\n\n")
  }

  sealed trait SkinParam extends PlantUml

  object SkinParam {
    case class Single(key: String, value: String) extends SkinParam

    case class Bundle(entity: String, stereotype: Option[String], xs: List[Single]) extends SkinParam
  }

  sealed trait Entity extends PlantUml

  private def oneThing(thing: String, name: String, title: Option[String], tag: Option[String]) = {
    val maybeTitle =
      title.fold(List.empty[String])(s => List(s"\"$s\"", "as"))

    val maybeTag =
      tag.fold(List.empty[String])(s => List(s"<< $s >>"))

    (thing :: maybeTitle ::: name :: maybeTag)
      .mkString(" ")
  }

  private def consumeOne(x: PlantUml): List[String] =
    x match {
      case Component(name, title, tag) =>
        oneThing("component", name, title, tag).list

      case Link(src, dest, length, weight, direction, oColor, oComment) =>
        val (segment, style) =
          weight match {
            case Link.Weight.Solid =>
              "-" -> Nil

            case Link.Weight.Dotted =>
              "." -> Nil

            case Link.Weight.Bold =>
              "-" -> List("bold")
          }

        val styles =
          style ++ oColor.map(s => "#" + s).toList

        val stylesStr =
          if (styles.isEmpty)
            ""
          else
            "[" + styles.mkString(",") + "]"

        val (left, right) =
          direction match {
            case Link.Direction.Forwards =>
              "" -> ">"

            case Link.Direction.Backwards =>
              "<" -> ""

            case Link.Direction.Bidirectional =>
              "<" -> ">"
          }

        val remainingLength =
          segment * (length - 1)

        (s"$src $left$segment$stylesStr$remainingLength$right $dest" :: oComment.toList)
          .mkString(" : ")
          .list

      case Queue(name, title, tag) =>
        oneThing("queue", name, title, tag).list

      case Database(name, title, tag, xs) =>
        val header =
          oneThing("database", name, title, tag)

        xs match {
          case Nil =>
            header.list

          case _ =>
            val open =
              header + " {"

            val close =
              "}"

            open :: xs.flatMap(consumeOne) appended close
        }

      case Package(title, xs) =>
        s"package \"$title\" {" :: xs.flatMap(consumeOne).map("  " + _).mkString("\n\n").list appended "}"

      case Actor(name, title, tag) =>
        oneThing("actor", name, title, tag).list

      case UseCase(name, title, tag) =>
        oneThing("usecase", name, title, tag).list

      case SkinParam.Single(k, v) =>
        s"skinparam $k $v".list

      case SkinParam.Bundle(entity, oStereotype, xs) =>
        val stereotype =
          oStereotype.fold("")(s => s"<< $s >>")

        (s"skinparam $entity$stereotype {" :: xs
          .map { case SkinParam.Single(k, v) => s"  $k $v" } appended "}")
          .mkString("\n").list
    }

  case class Component(name: String, title: Option[String], tag: Option[String]) extends Entity {
    def of(stereotype: String): Component =
      this.copy(tag = stereotype.some)
  }

  object Component {
    def apply(name: String): Component =
      Component(name, None, None)
  }

  case class Link(
      src: String,
      dest: String,
      length: Int,
      weight: Link.Weight,
      direction: Link.Direction,
      color: Option[String],
      comment: Option[String]
  ) extends PlantUml

  object Link {
    sealed trait Weight

    object Weight {
      case object Solid extends Weight
      case object Dotted extends Weight
      case object Bold extends Weight
    }

    sealed trait Direction

    object Direction {
      case object Forwards extends Direction
      case object Backwards extends Direction
      case object Bidirectional extends Direction
    }

    def apply(src: String, dest: String): Link =
      Link(src, dest, 2, Weight.Solid, Direction.Forwards, None, None)
  }

  case class Queue(name: String, title: Option[String], tag: Option[String]) extends Entity

  case class Database(name: String, title: Option[String], tag: Option[String], xs: List[PlantUml]) extends Entity

  case class Package(title: String, xs: List[PlantUml]) extends Entity

  case class Actor(name: String, title: Option[String], tag: Option[String]) extends Entity

  case class UseCase(name: String, title: Option[String], tag: Option[String]) extends Entity
}

sealed trait PlantUml
