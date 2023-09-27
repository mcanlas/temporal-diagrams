package com.htmlism.temporaldiagrams.demo.v2

import scala.util.chaining._

import cats.data.NonEmptyList
import cats.effect._
import cats.syntax.all._

import com.htmlism.temporaldiagrams.demo.FilePrinterAlg
import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.v2.Renderable
import com.htmlism.temporaldiagrams.v2.syntax.RenderableOps

object WriteOuroborosDiagram extends WriteOuroborosDiagram[IO](FilePrinterAlg[IO]) with IOApp.Simple {
  val diagram =
    NonEmptyList
      .of(
        OuroborosDsl.Type("User.Config"),
        OuroborosDsl.Type("User.Dsl"),
        OuroborosDsl.Type("TemporalDiagrams.PlantUml"),
        OuroborosDsl.Type("TemporalDiagrams.Mermaid"),
        OuroborosDsl.Output("Plantuml"),
        OuroborosDsl.Output("Mermaid"),
        OuroborosDsl.Encoding("User.Config", "User.Dsl", "Episode 1"),
        OuroborosDsl.Encoding("User.Config", "User.Dsl", "Episode 2"),
        OuroborosDsl.Encoding("User.Dsl", "TemporalDiagrams.PlantUml", "HighlightEncoder[PlantUml, User.Dsl]"),
        OuroborosDsl.Encoding("User.Dsl", "TemporalDiagrams.Mermaid", "HighlightEncoder[Mermaid, User.Dsl]"),
        OuroborosDsl.Encoding("TemporalDiagrams.PlantUml", "Plantuml", "DiagramEncoder[Plantuml]"),
        OuroborosDsl.Encoding("TemporalDiagrams.Mermaid", "Mermaid", "DiagramEncoder[Mermaid]")
      )
      .map(_.r)
}

// TODO need left to write support
class WriteOuroborosDiagram[F[_]](out: FilePrinterAlg[F]) {
  def run: F[Unit] =
    out.print("ouroboros.puml")(
      Renderable
        .renderMany(WriteOuroborosDiagram.diagram)
        .pipe(PlantUml.render(_))
        .mkString_("\n")
    )
}
