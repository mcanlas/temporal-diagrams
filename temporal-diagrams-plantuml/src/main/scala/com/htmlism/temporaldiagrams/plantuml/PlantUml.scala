package com.htmlism.temporaldiagrams.plantuml

import scala.util.chaining.*

import cats.*
import cats.Order.*
import cats.data.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.*

sealed trait PlantUml

object PlantUml:
  given DiagramEncoder[PlantUml] with
    def encode(x: PlantUml): Chain[String] =
      x match
        case x: Entity =>
          DiagramEncoder[PlantUml.Entity].encode(x)

        case x: Link =>
          DiagramEncoder[PlantUml.Link].encode(x)

        case x: Directive =>
          DiagramEncoder[PlantUml.Directive].encode(x)

  // TODO test
  case class ComponentDiagram(
      directives: Set[PlantUml.Directive] = Set.empty,
      entities: Set[PlantUml.Entity]      = Set.empty,
      links: Set[Link]                    = Set.empty
  ):
    def add(x: PlantUml): ComponentDiagram =
      this |+| ComponentDiagram(x)

    def +(x: PlantUml): ComponentDiagram =
      this |+| ComponentDiagram(x)

  object ComponentDiagram:
    val empty: ComponentDiagram =
      ComponentDiagram(Set.empty, Set.empty, Set.empty)

    given Monoid[ComponentDiagram] with
      def empty: ComponentDiagram =
        ComponentDiagram.empty

      def combine(x: ComponentDiagram, y: ComponentDiagram): ComponentDiagram =
        ComponentDiagram(
          x.directives ++ y.directives,
          x.entities ++ y.entities,
          x.links ++ y.links
        )

    /**
      * A collection builder style factory method for constructing [[ComponentDiagram]] where the user doesn't need to
      * keep track of the different diagram parts
      */
    def apply(xs: PlantUml*): ComponentDiagram =
      ComponentDiagram(
        xs.collect { case x: PlantUml.Directive => x }.toSet,
        xs.collect { case x: PlantUml.Entity => x }.toSet,
        xs.collect { case x: PlantUml.Link => x }.toSet
      )

    /**
      * A factory method that accepts a collection. Useful when complex inputs are constructed conditionally, rather
      * than specified inline
      */
    def apply[F[_]: Foldable: Functor](xs: F[PlantUml]): ComponentDiagram =
      xs
        .map(ComponentDiagram(_))
        .fold

  private val quoteTriggers =
    List("-", " ", "\\")

  private def safeQuote(s: String) =
    val encodedNewLines =
      s.replace("\n", "\\n")

    if quoteTriggers.exists(encodedNewLines.contains) then "\"" + encodedNewLines + "\""
    else encodedNewLines

  private def id(s: String) =
    s
      .replace("-", "_")
      .replace(" ", "_")

  private def renderSubsectionSorted[A: DiagramEncoder](xs: Set[A]) =
    xs.toList.map(summon[DiagramEncoder[A]].encode).sorted

  // if links refer to a component that isn't defined, they will implicitly create their own;
  // so allow components to take priority
  def render(x: PlantUml.ComponentDiagram): Chain[String] =
    Chain(
      x.directives.pipe(renderSubsectionSorted),
      x.entities.pipe(renderSubsectionSorted),
      x.links.pipe(renderSubsectionSorted)
    )
      .filter(_.nonEmpty)           // drop empty sections
      .map(Chain.fromSeq)           // from list to chain
      .flatten                      // make them one stream of bundles
      .pipe(interlace(_, identity)) // intersperse newlines and flatten
      .pipe(asDocument)

  /**
    * Something that isn't a link and isn't an entity
    */
  sealed trait Directive extends PlantUml

  object Directive:
    given directiveEncoder: DiagramEncoder[Directive] with
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

          case Title(xs) =>
            if xs.isEmpty then Chain.empty
            else
              Chain.one:
                s"title ${xs.mkString("\n")}"

          case Caption(xs) =>
            if xs.isEmpty then Chain.empty
            else
              Chain.one:
                s"caption ${xs.mkString("\n")}"

          case Legend(xs) =>
            if xs.isEmpty then Chain.empty
            else
              Chain
                .fromSeq(xs)
                .prepend("legend")
                .append("endlegend")

  case object LeftToRightDirection extends Directive

  // something that can be nested in a package; or is global in scope, like an arrow
  sealed trait Entity extends PlantUml

  object Entity:
    sealed trait HasAlias[A <: HasAlias[A]] extends Entity:
      /**
        * Optional. If specified, arrows can refer to this component by this alias. If not provided, the alias will be
        * the value of `name`
        */
      def alias: Option[String]

      def withAlias(s: String): A

    sealed trait HasStereotype[A <: HasStereotype[A]] extends Entity:
      /**
        * Optional. A tag to share styling among components of the same stereotype
        */
      def stereotype: Option[String]

      def withStereotype(s: String): A

    given entityEncoder: DiagramEncoder[Entity] with
      def encode(x: Entity): Chain[String] =
        x match
          case Package(name, xs) =>
            xs
              .pipe(renderSubsectionSorted)
              .pipe(Chain.fromSeq)
              .pipe(interlace(_, identity))
              .map { s =>
                if s.isEmpty then ""
                else "  " + s
              }
              .prepend(s"package ${safeQuote(name)} {")
              .append("}")

          case Component(name, oAlias, oStereotype) =>
            Chain:
              entity("component", name, oAlias, oStereotype)

          case Actor(name, oAlias, oStereotype) =>
            Chain:
              entity("actor", name, oAlias, oStereotype)

          case BusinessActor(name, oAlias, oStereotype) =>
            Chain:
              entity("actor/", name, oAlias, oStereotype)

          case Queue(name, oAlias, oStereotype) =>
            Chain:
              entity("queue", name, oAlias, oStereotype)

          case UseCase(name, oAlias, oStereotype) =>
            Chain:
              entity("usecase", name, oAlias, oStereotype)

          case BusinessUseCase(name, oAlias, oStereotype) =>
            Chain:
              entity("usecase/", name, oAlias, oStereotype)

          case Database(name, oAlias, oStereotype, xs) =>
            val slug =
              entity("database", name, oAlias, oStereotype)

            if xs.isEmpty then
              Chain:
                slug
            else
              xs
                .pipe(renderSubsectionSorted)
                .pipe(Chain.fromSeq)
                .pipe(interlace(_, identity))
                .map { s =>
                  if s.isEmpty then ""
                  else "  " + s
                }
                .prepend(s"$slug {")
                .append("}")

          case Interface(name, oAlias) =>
            Chain:
              entity("interface", name, oAlias, None)

  private def entity(
      componentType: String,
      name: String,
      oAlias: Option[String]      = None,
      oStereotype: Option[String] = None
  ): String =
    s"$componentType ${safeQuote(name)}"
      .applySome(oAlias)((s, a) => s + s" as ${id(a)}")
      .applySome(oStereotype)((s, st) => s + s" << $st >>")

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
  case class Component(name: String, alias: Option[String] = None, stereotype: Option[String] = None)
      extends Entity.HasAlias[Component],
        Entity.HasStereotype[Component]:
    def withAlias(s: String): Component =
      copy(alias = s.some)

    def withStereotype(s: String): Component =
      copy(stereotype = s.some)

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
  case class Actor(name: String, alias: Option[String] = None, stereotype: Option[String] = None)
      extends Entity.HasAlias[Actor],
        Entity.HasStereotype[Actor]:
    def withAlias(s: String): Actor =
      copy(alias = s.some)

    def withStereotype(s: String): Actor =
      copy(stereotype = s.some)

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
  case class BusinessActor(name: String, alias: Option[String] = None, stereotype: Option[String] = None)
      extends Entity.HasAlias[BusinessActor],
        Entity.HasStereotype[BusinessActor]:
    def withAlias(s: String): BusinessActor =
      copy(alias = s.some)

    def withStereotype(s: String): BusinessActor =
      copy(stereotype = s.some)

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
  case class Queue(name: String, alias: Option[String] = None, stereotype: Option[String] = None)
      extends Entity.HasAlias[Queue],
        Entity.HasStereotype[Queue]:
    def withAlias(s: String): Queue =
      copy(alias = s.some)

    def withStereotype(s: String): Queue =
      copy(stereotype = s.some)

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
  case class UseCase(name: String, alias: Option[String] = None, stereotype: Option[String] = None)
      extends Entity.HasAlias[UseCase],
        Entity.HasStereotype[UseCase]:
    def withAlias(s: String): UseCase =
      copy(alias = s.some)

    def withStereotype(s: String): UseCase =
      copy(stereotype = s.some)

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
  case class BusinessUseCase(name: String, alias: Option[String] = None, stereotype: Option[String] = None)
      extends Entity.HasAlias[BusinessUseCase],
        Entity.HasStereotype[BusinessUseCase]:
    def withAlias(s: String): BusinessUseCase =
      copy(alias = s.some)

    def withStereotype(s: String): BusinessUseCase =
      copy(stereotype = s.some)

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
  case class Database(
      name: String,
      alias: Option[String]      = None,
      stereotype: Option[String] = None,
      xs: Set[Entity]            = Set.empty
  ) extends Entity.HasAlias[Database],
        Entity.HasStereotype[Database]:
    def withAlias(s: String): Database =
      copy(alias = s.some)

    def withStereotype(s: String): Database =
      copy(stereotype = s.some)

  /**
    * A building block in component diagrams
    *
    * @param name
    *   This text is visible in the diagram
    * @param alias
    *   Optional. If provided, arrows can refer to this component by this alias. If not provided, the alias will be the
    *   value of `name`
    */
  // TODO
  case class Interface(name: String, alias: Option[String]) extends Entity.HasAlias[Interface]:
    def withAlias(s: String): Interface =
      copy(alias = s.some)

  /**
    * A link from the source to the destination
    *
    * In terms of "gravity", the source is always first. In top-down diagrams, the source is on the top and the
    * destination is on the bottom. In left-to-right diagrams, the source is on the left and the destination is on the
    * right.
    *
    * @param source
    *   The base side of the link, where it originates. The source has higher rank
    * @param destination
    *   The tip side of the link, where it stops. The destination has lower rank
    * @param length
    *   The amount of rank a link has influence over, measured in segments in the markup. If length = 1, the linked
    *   components have the same rank
    * @param direction
    *   The direction of arrows on the link
    * @param weight
    *   The style and thickness of the arrow's body
    * @param influencesRank
    *   If true, the link will affect the rank between the linked components
    * @param isVisible
    *   If true, the link will be rendered and occupy space; if false, the link will occupy space and influence rank but
    *   transparently
    * @param text
    *   Optional text written along the link
    * @param color
    *   Optional color value, without the leading `#`. See [[https://plantuml.com/color]]
    */
  case class Link(
      source: String,
      destination: String,
      length: Int,
      direction: Link.Direction,
      weight: Link.Weight,
      influencesRank: Boolean,
      isVisible: Boolean,
      text: Option[String]  = None,
      color: Option[String] = None
  ) extends PlantUml:
    def withText(s: String): Link =
      copy(text = Some(s))

  object Link:
    def apply(source: String, destination: String): Link =
      Link(
        source,
        destination,
        length = 2,
        Link.Direction.Forwards,
        Link.Weight.Solid,
        influencesRank = true,
        isVisible      = true,
        text           = None,
        color          = None
      )

    enum Direction:
      case Forwards
      case Backwards
      case Bidirectional
      case Empty

    enum Weight:
      case Solid
      case Dotted
      case Bold

    given linkEncoder: DiagramEncoder[Link] with
      def encode(x: Link): Chain[String] =
        x match
          case Link(src, dest, length, dir, weight, influencesRank, isVisible, oText, oColor) =>
            val (segment, weightStyle) =
              weight match
                case Link.Weight.Solid =>
                  "-" -> Nil

                case Link.Weight.Dotted =>
                  "." -> Nil

                case Link.Weight.Bold =>
                  "-" -> List("bold")

            val bodyHead =
              segment

            val bodyTail =
              segment * (length - 1)

            val colorStyle =
              oColor
                .map("#" + _)
                .toList

            val rankStyle =
              if influencesRank then Nil
              else List("norank")

            val hiddenStyle =
              if isVisible then Nil
              else List("hidden")

            val styles =
              weightStyle ++ colorStyle ++ rankStyle ++ hiddenStyle

            val stylesStr =
              if styles.isEmpty then ""
              else "[" + styles.mkString(",") + "]"

            val (leftHead, rightHead) =
              dir match
                case Link.Direction.Forwards =>
                  "" -> ">"

                case Link.Direction.Backwards =>
                  "<" -> ""

                case Link.Direction.Bidirectional =>
                  "<" -> ">"

                case Link.Direction.Empty =>
                  "" -> ""

            Chain:
              s"${safeQuote(src)} $leftHead$bodyHead$stylesStr$bodyTail$rightHead ${safeQuote(dest)}"
                .applySome(oText)((s, t) => s"$s : $t")

  // TODO test
  case class SkinParam(key: String, value: String) extends Directive

  case class SkinParamGroup(base: String, parameters: List[SkinParamGroup.Parameter], stereotype: Option[String] = None)
      extends Directive:
    def and(key: String, value: String): SkinParamGroup =
      this.copy(parameters = parameters.appended(SkinParamGroup.Parameter(key, value)))

  object SkinParamGroup:
    case class Parameter(name: String, value: String)

    def apply(base: String): SkinParamGroup =
      SkinParamGroup(base, Nil, None)

    def apply(base: String, stereotype: String): SkinParamGroup =
      SkinParamGroup(base, Nil, stereotype.some)

  case class Package(name: String, xs: Set[Entity]) extends Entity

  object Package:
    def apply(name: String, xs: Entity*): Package =
      Package(name, xs.toSet)

  private def asDocument(xs: Chain[String]) =
    Chain("@startuml", "") ++
      xs ++
      Chain("", "@enduml")

  case class Title(xs: List[String]) extends Directive

  case class Caption(xs: List[String]) extends Directive

  case class Legend(xs: List[String]) extends Directive
