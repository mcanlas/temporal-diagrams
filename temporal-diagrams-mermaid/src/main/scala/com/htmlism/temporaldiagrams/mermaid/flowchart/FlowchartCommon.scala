package com.htmlism.temporaldiagrams.mermaid.flowchart

trait FlowchartCommon:
  def entities: Set[FlowchartDsl.Entity]
  def links: Set[FlowchartDsl.Link]
