package com.htmlism.temporaldiagrams.mermaid
package flowchart

class LeftToRight

object LeftToRight:
  given MermaidDiagramType[LeftToRight] with
    def header: String =
      "flowchart LR"
