package com.htmlism.temporaldiagrams.v2

import cats.Eq
import cats.data.*

sealed trait ToyDiagramLanguage

object ToyDiagramLanguage:
  case class Component(s: String) extends ToyDiagramLanguage

  case class Arrow(s: String) extends ToyDiagramLanguage

  implicit val toyEncoder: DiagramEncoder[ToyDiagramLanguage] =
    (x: ToyDiagramLanguage) =>
      val str =
        x match
          case Component(s) =>
            s"component($s)"
          case Arrow(s) =>
            s"arrow($s)"

      NonEmptyChain.one(str)

  implicit val toyEq: Eq[ToyDiagramLanguage] =
    Eq.fromUniversalEquals

  implicit def necHighlightEncoder[A](using
      enc: HighlightEncoder[ToyDiagramLanguage, A]
  ): HighlightEncoder[NonEmptyChain[ToyDiagramLanguage], A] =
    new HighlightEncoder[NonEmptyChain[ToyDiagramLanguage], A]:
      def encode(x: A): NonEmptyChain[ToyDiagramLanguage] =
        NonEmptyChain.one(enc.encode(x))

      def encodeWithHighlights(x: A, highlighted: Boolean): NonEmptyChain[ToyDiagramLanguage] =
        NonEmptyChain.one(enc.encodeWithHighlights(x, highlighted))
