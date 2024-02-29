package com.htmlism.temporaldiagrams.mermaid.flowchart

trait FlowchartFactory[A](f: (Set[FlowchartDeclaration.Node], Set[FlowchartDeclaration.Link]) => A):
  def empty: A =
    f(Set.empty, Set.empty)

  def apply(xs: FlowchartDeclaration*): A =
    f(
      xs.collect { case x: FlowchartDeclaration.Node => x }.toSet,
      xs.collect { case x: FlowchartDeclaration.Link => x }.toSet
    )
