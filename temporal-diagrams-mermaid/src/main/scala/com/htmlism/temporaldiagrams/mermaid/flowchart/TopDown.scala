package com.htmlism.temporaldiagrams.mermaid
package flowchart

class TopDown

object TopDown:
  given MermaidDiagramType[TopDown] with
    def header: String =
      "flowchart TD"
