package com.htmlism.temporaldiagrams
package demo

import scala.util.chaining._

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._

import com.htmlism.temporaldiagrams.syntax._

object Demo extends App {
  val producer =
    FacetedFrame.from[Int, DemoDsl]("producer",
      1 -> (Service("foo", None)).tag("foo"),
      2 -> Service("new_foo", None).r
    )

  val consumer =
    FacetedFrame.from[Int, DemoDsl]("consumer",
      1 -> Service("bar", "foo".some).tag("bar"),
      2 -> Service("bar", "new_foo".some).tag("bar"),
      3 -> Hydra("bar", "new_foo".some).tag("bar"),
      4 -> Buffered("new_bar", "foo".some).r
    )

  val everything =
    Nel.of(producer, consumer)

  val narrative =
    everything
      .start
      .next("producer" -> 2, "consumer" -> 2)
      .next("consumer" -> 3)
      .reset("consumer" -> 4)

  for {
    pair <- narrative.episodes.zipWithIndex.toList
  } {
    val (manyR, i) = pair

    val one =
      "" -> ((_: Renderable[DemoDsl]).encodeAs[PlantUml])

    val highlights =
      manyR.toList.flatMap(_.keys).map { s =>
        s"-$s" -> ((_: Renderable[DemoDsl]).encodeWithHighlightsOn[PlantUml](s))
      }

    (one :: highlights)
      .foreach { f =>
        val slug =
          f._1

        val payload =
          manyR
            .toList
            .flatMap(f._2)
            .pipe(PlantUml.render)

        FilePrinterAlg[IO].print(i.toString + slug + ".puml")(payload)
          .unsafeRunSync()
      }
  }
}
