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
      OuroborosDsl.Output("Mermaid")
    )
}

class WriteOuroborosDiagram[F[_]: Applicative] {
  def run: F[Unit] =
    Applicative[F].unit
}
