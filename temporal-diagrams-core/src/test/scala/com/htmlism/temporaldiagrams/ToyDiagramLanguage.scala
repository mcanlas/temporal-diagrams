package com.htmlism.temporaldiagrams

import cats.Eq
import cats.data.*

sealed trait ToyDiagramLanguage

object ToyDiagramLanguage:
  case class Component(s: String) extends ToyDiagramLanguage

  case class Arrow(s: String) extends ToyDiagramLanguage

  given DiagramEncoder[Chain[ToyDiagramLanguage]] =
    (xs: Chain[ToyDiagramLanguage]) =>
      xs.map:
        case Component(s) =>
          s"component($s)"
        case Arrow(s) =>
          s"arrow($s)"

  given Eq[ToyDiagramLanguage] =
    Eq.fromUniversalEquals

  given [A](using
      enc: HighlightEncoder[ToyDiagramLanguage, A]
  ): HighlightEncoder[Chain[ToyDiagramLanguage], A] =
    new HighlightEncoder[Chain[ToyDiagramLanguage], A]:
      def encode(x: A): Chain[ToyDiagramLanguage] =
        Chain(enc.encode(x))

      def encodeWithHighlights(x: A, highlighted: Boolean): Chain[ToyDiagramLanguage] =
        Chain(enc.encodeWithHighlights(x, highlighted))
