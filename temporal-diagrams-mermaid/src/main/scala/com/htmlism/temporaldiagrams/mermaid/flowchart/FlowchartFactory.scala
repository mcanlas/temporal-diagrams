package com.htmlism.temporaldiagrams.mermaid.flowchart

trait FlowchartFactory[A](f: (Set[FlowchartDsl.Entity], Set[FlowchartDsl.Link]) => A):
  def empty: A =
    f(Set.empty, Set.empty)

  def apply(xs: FlowchartDsl*): A =
    f(
      xs.collect { case x: FlowchartDsl.Entity => x }.toSet,
      xs.collect { case x: FlowchartDsl.Link => x }.toSet
    )
