package com.htmlism.temporaldiagrams
package demo

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._

import com.htmlism.temporaldiagrams.syntax._

object Demo extends App {
  val producer =
    TemporalFrame[Int, DemoDsl](
      1 -> (Service("foo", None)).id("foo"),
      2 -> Service("new_foo", None).r
    )

  val consumer =
    TemporalFrame[Int, DemoDsl](
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

        FilePrinterAlg[IO].print(k.toString + slug + ".puml")(payload)
          .unsafeRunSync()
      }
  }
}
