package com.htmlism.temporaldiagrams.mermaid
package flowchart

class BottomToTop

object BottomToTop:
  given MermaidDiagramType[BottomToTop] with
    def header: String =
      "flowchart BT"
