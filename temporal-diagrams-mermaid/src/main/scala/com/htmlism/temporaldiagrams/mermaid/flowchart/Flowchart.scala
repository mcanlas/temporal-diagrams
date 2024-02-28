package com.htmlism.temporaldiagrams.mermaid
package flowchart

sealed trait Flowchart

object Flowchart:
  given MermaidDiagramType[Flowchart] with
    def header: String =
      "flowchart"
