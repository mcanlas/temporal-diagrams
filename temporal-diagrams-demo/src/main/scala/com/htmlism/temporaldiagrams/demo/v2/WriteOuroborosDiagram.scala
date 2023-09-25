package com.htmlism.temporaldiagrams.demo.v2

import cats.Applicative
import cats.effect._

object WriteOuroborosDiagram extends WriteOuroborosDiagram[IO] with IOApp.Simple

class WriteOuroborosDiagram[F[_]: Applicative] {
  def run: F[Unit] =
    Applicative[F].unit
}
