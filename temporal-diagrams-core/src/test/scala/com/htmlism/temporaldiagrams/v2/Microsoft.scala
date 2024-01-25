package com.htmlism.temporaldiagrams.v2

import cats.data.Chain

enum Microsoft:
  case Arrow(src: String, dest: String)

object Microsoft:
  given MultiArrowEncoder[String, Microsoft.Arrow] with
    def encodeArrow(source: String, destination: String): Microsoft.Arrow =
      Arrow(source, destination)

  given HighlightEncoder[Chain[ToyDiagramLanguage], Microsoft.Arrow] with
    def encode(x: Microsoft.Arrow): Chain[ToyDiagramLanguage] =
      Chain:
        ToyDiagramLanguage.Arrow(s"${x.src} to ${x.dest}")

    def encodeWithHighlights(x: Microsoft.Arrow, highlighted: Boolean): Chain[ToyDiagramLanguage] =
      Chain:
        ToyDiagramLanguage.Arrow(s"${x.src} to ${x.dest}")
