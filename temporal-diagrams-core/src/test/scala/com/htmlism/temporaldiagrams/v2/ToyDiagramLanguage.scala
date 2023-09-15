package com.htmlism.temporaldiagrams.v2

import cats.data.NonEmptyList

trait ToyDiagramLanguage

object ToyDiagramLanguage {
  case class Component(s: String)

  object Component {
    implicit val componentEncoder: DiagramEncoder[ToyDiagramLanguage, Component] =
      (x: Component) => NonEmptyList.one(s"component(${x.s})")
  }
}
