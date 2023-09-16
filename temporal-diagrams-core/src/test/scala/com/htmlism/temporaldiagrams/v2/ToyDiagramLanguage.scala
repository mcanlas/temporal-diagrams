package com.htmlism.temporaldiagrams.v2

import cats.Eq
import cats.data.NonEmptyList

sealed trait ToyDiagramLanguage

object ToyDiagramLanguage {
  case class Component(s: String) extends ToyDiagramLanguage

  case class Arrow(s: String) extends ToyDiagramLanguage

  implicit val toyEncoder: DiagramEncoder[ToyDiagramLanguage] =
    (x: ToyDiagramLanguage) => {
      val str =
        x match {
          case Component(s) =>
            s"component($s)"
          case Arrow(s) =>
            s"arrow($s)"
        }

      NonEmptyList.one(str)
    }

  implicit val toyEq: Eq[ToyDiagramLanguage] =
    Eq.fromUniversalEquals
}
