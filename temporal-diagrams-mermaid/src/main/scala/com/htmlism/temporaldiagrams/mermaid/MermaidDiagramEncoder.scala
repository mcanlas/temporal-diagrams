package com.htmlism.temporaldiagrams.mermaid

import cats.data.Chain

trait MermaidDiagramEncoder[A]:
  def header(x: A): String

  def encode(x: A): Chain[String]
