package com.htmlism.temporaldiagrams
package demo

import cats.data.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.mermaid.*
import com.htmlism.temporaldiagrams.mermaid.flowchart.FlowchartDsl.*
import com.htmlism.temporaldiagrams.plantuml.*

sealed trait DemoDsl

object DemoDsl:
  case class Lambda(name: String)                  extends DemoDsl
  case class Service(name: String)                 extends DemoDsl
  case class Database(name: String, replicas: Int) extends DemoDsl
  case class Title(s: String)                      extends DemoDsl

  case class Arrow(src: String, dest: String, text: Option[String] = None)

  object Arrow:
    given BrightEncoder[PlantUml.ComponentDiagram, Arrow] with
      def encodeBrightly(x: Arrow, isBright: Boolean): PlantUml.ComponentDiagram =
        val Arrow(src, dest, oText) = x

        PlantUml.ComponentDiagram:
          PlantUml.Link(src, dest).copy(text = oText)

    given BrightEncoder[MermaidDiagram[Flowchart], Arrow] with
      def encodeBrightly(x: Arrow, isBright: Boolean): MermaidDiagram[Flowchart] =
        val Arrow(src, dest, oText) = x

        MermaidDiagram.of:
          Flowchart(
            Link.LinkChain(
              NonEmptyList.one(src),
              NonEmptyList.of(
                Link
                  .Segment
                  .Visible(
                    1,
                    Link.Weight.Normal,
                    Link.Direction.Single(Link.Head.Arrow),
                    NonEmptyList.one(dest),
                    text = oText
                  )
              )
            )
          )

  given BrightEncoder[PlantUml.ComponentDiagram, DemoDsl] with
    def encodeBrightly(x: DemoDsl, isBright: Boolean): PlantUml.ComponentDiagram =
      x match
        case Lambda(name) =>
          PlantUml.ComponentDiagram(
            PlantUml.Component(name, None, Option.when(isBright)("Lambda")),
            if isBright then skinPlantUmlRed("component", "Lambda") else skinPlantUmlWhite("component"),
            PlantUml.Link(name, "database").withText("writes to")
          )

        case Service(name) =>
          PlantUml.ComponentDiagram(
            PlantUml.Component(name, None, Option.when(isBright)("Service")),
            if isBright then skinPlantUmlBlue("component", "Service") else skinPlantUmlWhite("component")
          )

        case Database(name, numReplicas) =>
          val replicas =
            (1 to numReplicas)
              .map: n =>
                PlantUml.Database(s"replica-$n", None, Option.when(isBright)("Replica"), xs = Set.empty)
              .toSet[PlantUml.Entity]

          val replicaSkin =
            if isBright then skinPlantUmlGray("database", "Replica") else skinPlantUmlWhite("database")

          PlantUml
            .ComponentDiagram(
              PlantUml.Package(
                "Persistence",
                replicas + PlantUml.Database(name, None, Option.when(isBright)("Database"), xs = Set.empty)
              ),
              if isBright then skinPlantUmlYellow("database", "Database") else skinPlantUmlWhite("database"),
              replicaSkin
            )

        case Title(s) =>
          PlantUml.ComponentDiagram:
            PlantUml.Title(List(s))

  given BrightEncoder[MermaidDiagram[Flowchart], DemoDsl] with
    def encodeBrightly(x: DemoDsl, isBright: Boolean): MermaidDiagram[Flowchart] =
      x match
        case Lambda(name) =>
          MermaidDiagram.of:
            Flowchart:
              Link.LinkChain(
                NonEmptyList.one(name),
                NonEmptyList.of(
                  Link
                    .Segment
                    .Visible(
                      1,
                      Link.Weight.Normal,
                      Link.Direction.Single(Link.Head.Arrow),
                      NonEmptyList.one("database"),
                      text = "writes to".some
                    )
                )
              ) ::
                Node.Simple(name, nodeClass = Option.when(isBright)("Lambda")) ::
                skinMermaidRed(isBright)

        case Service(name) =>
          MermaidDiagram.of:
            Flowchart:
              Node.Simple(name, nodeClass = Option.when(isBright)("Service")) ::
                skinMermaidBlue(isBright)

        case Database(name, numReplicas) =>
          val replicas =
            (1 to numReplicas)
              .map: n =>
                Node.WithShape(
                  s"replica-$n",
                  Node.Shape.Cylinder,
                  nodeClass = Option.when(isBright)("Database")
                )
              .toList

          val primary =
            Node.WithShape(name, Node.Shape.Cylinder, nodeClass = Option.when(isBright)("Database"))

          MermaidDiagram.of:
            Flowchart:
              Subgraph(
                id           = "persistence",
                text         = "Persistence".some,
                direction    = None,
                declarations = ((replicas :+ primary) ::: skinMermaidBlue(isBright)).toSet,
                links        = Set.empty
              )
        case Title(s) =>
          MermaidDiagram(Chain(FrontMatterPair.StringPair("title", s)), Flowchart.empty)

  private def skinPlantUmlYellow(name: String, stereotype: String) =
    PlantUml
      .SkinParamGroup(name, stereotype)
      .and("fontStyle", "bold")
      .and("fontColor", "#444")
      .and("backgroundColor", "#faf2c8/#e6c72c")
      .and("borderColor", "#807746")
      .and("borderThickness", "2")

  private def skinPlantUmlBlue(name: String, stereotype: String) =
    PlantUml
      .SkinParamGroup(name, stereotype)
      .and("fontStyle", "bold")
      .and("fontColor", "white")
      .and("backgroundColor", "#7082b8/#283d7a")
      .and("borderColor", "#223336")
      .and("borderThickness", "2")

  private def skinPlantUmlWhite(stereotype: String) =
    PlantUml
      .SkinParamGroup(stereotype)
      .and("fontStyle", "bold")
      .and("fontColor", "#AAA")
      .and("backgroundColor", "white")
      .and("borderColor", "#AAA")
      .and("borderThickness", "2")

  private def skinPlantUmlGray(name: String, stereotype: String) =
    PlantUml
      .SkinParamGroup(name, stereotype)
      .and("fontStyle", "bold")
      .and("fontColor", "#333")
      .and("backgroundColor", "white/#CCC")
      .and("borderColor", "#888")
      .and("borderThickness", "2")

  private def skinPlantUmlRed(name: String, stereotype: String) =
    PlantUml
      .SkinParamGroup(name, stereotype)
      .and("fontStyle", "bold")
      .and("fontColor", "white")
      .and("backgroundColor", "#d47777/#822323")
      .and("borderColor", "#642a2a")
      .and("borderThickness", "2")

  private def skinMermaidBlue(isBright: Boolean) =
    Option
      .when(isBright):
        ClassDef(NonEmptyList.one("Service"), NonEmptyList.of("fill" -> "#a1add1"))
      .toList

  private def skinMermaidRed(isBright: Boolean) =
    Option
      .when(isBright):
        ClassDef(NonEmptyList.one("Lambda"), NonEmptyList.of("fill" -> "#d49090"))
      .toList

  case class Config(
      title: String,
      databaseReplicas: Int,
      serviceInstances: Int
  )
