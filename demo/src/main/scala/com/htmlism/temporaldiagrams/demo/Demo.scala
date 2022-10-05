package com.htmlism.temporaldiagrams
package demo

import scala.util.chaining._

import cats.data._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._

import com.htmlism.temporaldiagrams.syntax._

object Demo extends App {
  val producer =
    FacetedFrame.from[Int, DemoDsl](
      "producer",
      1 -> (Service("foo", None)).tag("foo").list,
      2 -> Service("new_foo", None).r.list
    )

  val consumer =
    FacetedFrame.from[Int, DemoDsl](
      "consumer",
      1 -> Service("bar", "foo".some).tag("bar").list,
      2 -> Service("bar", "new_foo".some).tag("bar").list,
      3 -> Hydra("bar", "new_foo".some).tag("bar").list,
      4 -> Buffered("new_bar", "foo".some).r.list
    )

  val everything =
    NonEmptyList.of(producer, consumer)

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
      "" -> (DslEncoder.encodeMany[DemoDsl, PlantUml](_))

    val highlights =
      manyR
        .flatMap(_.tags)
        .map(s =>
          s"-$s" -> (DslEncoder.encodeManyWithHighlights[DemoDsl, PlantUml](
            _: List[Renderable.Tagged[DemoDsl]],
            s
          ))
        )

    (one :: highlights)
      .foreach { f =>
        val slug =
          f._1

        val payload =
          manyR
            .pipe(f._2)
            .pipe(PlantUml.render)

        FilePrinterAlg[IO]
          .print(i.toString + slug + ".puml")(payload)
          .unsafeRunSync()
      }
  }
}
