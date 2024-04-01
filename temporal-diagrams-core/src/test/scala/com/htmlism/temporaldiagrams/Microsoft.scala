package com.htmlism.temporaldiagrams

import cats.data.Chain

enum Microsoft:
  case Arrow(src: String, dest: String)

object Microsoft:
  given HighlightEncoder[Chain[ToyDiagramLanguage], Microsoft.Arrow] with
    def encode(x: Microsoft.Arrow): Chain[ToyDiagramLanguage] =
      Chain:
        ToyDiagramLanguage.Arrow(s"${x.src} to ${x.dest}")

    def encodeWithHighlights(x: Microsoft.Arrow, highlighted: Boolean): Chain[ToyDiagramLanguage] =
      Chain:
        ToyDiagramLanguage.Arrow(s"${x.src} to ${x.dest}")
