package com.htmlism.temporaldiagrams.demo.v2

import cats.Applicative
import cats.data.NonEmptyList
import cats.effect._

object WriteOuroborosDiagram extends WriteOuroborosDiagram[IO] with IOApp.Simple {
  val diagram =
    NonEmptyList.of(
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
}

class WriteOuroborosDiagram[F[_]: Applicative] {
  def run: F[Unit] =
    Applicative[F].unit
}
