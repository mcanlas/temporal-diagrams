package com.htmlism.temporaldiagrams.demo.v2

import scala.util.chaining.*

import cats.data.NonEmptyChain
import cats.effect.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.demo.FilePrinterAlg
import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.v2.Renderable
import com.htmlism.temporaldiagrams.v2.syntax.RenderableOps

object WriteOuroborosDiagram extends WriteOuroborosDiagram[IO](FilePrinterAlg[IO]) with IOApp.Simple {
  val diagram =
    NonEmptyChain
      .of(
        OuroborosDsl.Type("User.Config"),
        OuroborosDsl.Type("User.Dsl"),
        OuroborosDsl.Type("TemporalDiagrams.PlantUml"),
        OuroborosDsl.Type("TemporalDiagrams.Mermaid"),
        OuroborosDsl.Output("PlantUml", "vertical"),
        OuroborosDsl.Output("PlantUml", "horizontal"),
        OuroborosDsl.Output("PlantUml", "with_highlights"),
        OuroborosDsl.Output("Mermaid", "default"),
        OuroborosDsl.Encoding("User.Config", "User.Dsl", "Episode 1"),
        OuroborosDsl.Encoding("User.Config", "User.Dsl", "Episode 2"),
        OuroborosDsl.Encoding("User.Dsl", "TemporalDiagrams.PlantUml", "HighlightEncoder[PlantUml, User.Dsl]"),
        OuroborosDsl.Encoding("User.Dsl", "TemporalDiagrams.Mermaid", "HighlightEncoder[Mermaid, User.Dsl]"),
        OuroborosDsl.Encoding("TemporalDiagrams.PlantUml", "PlantUml_vertical", "DiagramEncoder[Plantuml]\\nrender"),
        // TODO variants are upstream; this is not accurate
        OuroborosDsl.Encoding(
          "TemporalDiagrams.PlantUml",
          "PlantUml_horizontal",
          "DiagramEncoder[Plantuml]\\nrender horizontally"
        ),
        OuroborosDsl.Encoding(
          "TemporalDiagrams.PlantUml",
          "PlantUml_with_highlights",
          "DiagramEncoder[Plantuml]\\nrender with highlights"
        ),
        OuroborosDsl.Encoding("TemporalDiagrams.Mermaid", "Mermaid_default", "DiagramEncoder[Mermaid]")
      )
      .map(_.r)
}

class WriteOuroborosDiagram[F[_]](out: FilePrinterAlg[F]) {
  def run: F[Unit] =
    out.print("ouroboros.puml")(
      Renderable
        .renderMany(WriteOuroborosDiagram.diagram)
        .pipe(PlantUml.render)
        .mkString_("\n")
    )
}
