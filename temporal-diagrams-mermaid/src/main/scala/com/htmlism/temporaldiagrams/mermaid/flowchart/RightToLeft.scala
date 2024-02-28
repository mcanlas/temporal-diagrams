package com.htmlism.temporaldiagrams.mermaid
package flowchart

class RightToLeft

object RightToLeft:
  given MermaidDiagramType[RightToLeft] with
    def header: String =
      "flowchart RL"
