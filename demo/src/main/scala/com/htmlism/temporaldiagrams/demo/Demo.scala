package com.htmlism.temporaldiagrams
package demo

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._

import com.htmlism.temporaldiagrams.syntax._

object Demo extends App {
  val producer =
    Temporal[Int, DemoDsl](
      1 -> (Service("foo", None)).id("foo"),
      2 -> Service("new_foo", None).r
    )

  val consumer =
    Temporal[Int, DemoDsl](
      1 -> Service("bar", "foo".some).id("bar"),
      2 -> Service("bar", "new_foo".some).id("bar"),
      3 -> Hydra("bar", "new_foo".some).r,
      4 -> Buffered("new_bar", "new_foo".some).r
    )

  val everything =
    producer |+| consumer

  for {
    k <- everything.keys
  } {
    val one =
      "" -> (_: Renderable[DemoDsl]).renderAs[PlantUml]

    val highlights =
      everything.at(k).keys.map(s => s"-$s" -> (_: Renderable[DemoDsl]).renderWithHighlightsOn[PlantUml](s))

    (one :: highlights)
      .foreach { f =>
        val (slug, payload) = f(everything.at(k))

        FilePrinterAlg[IO].print(k + slug + ".puml")(wrap(payload))
          .unsafeRunSync()
      }
  }

  lazy val style =
    """
      |skinparam component<< Dim >> {
      |  fontColor #EEE
      |  backgroundColor #FFF
      |}
      |
      |skinparam component<< Highlighted >> {
      |  fontColor black
      |  backgroundColor #ffe696
      |}
      |
      |skinparam component {
      |  fontStyle bold
      |  fontColor white
      |  backgroundColor #586ba4
      |  borderColor #223336
      |  borderThickness 2
      |}""".stripMargin

  private def wrap(s: String) =
    List("@startuml", style, s, "@enduml")
      .mkString("\n")
}
