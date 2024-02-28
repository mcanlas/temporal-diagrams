package com.htmlism.temporaldiagrams.mermaid
package flowchart

class TopToBottom

object TopToBottom:
  given MermaidDiagramType[TopToBottom] with
    def header: String =
      "flowchart TB"
