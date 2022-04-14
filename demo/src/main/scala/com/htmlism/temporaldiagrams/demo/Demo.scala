package com.htmlism.temporaldiagrams
package demo

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._

import com.htmlism.temporaldiagrams.syntax._

object Demo extends App {
  val producer =
    Temporal(
      1 -> Service("foo", None).r,
      2 -> Service("new foo", None).r
    )

  val consumer =
    Temporal(
      1 -> Service("bar", Service("foo", None).some).r,
      3 -> Service("new bar", Service("foo", None).some).r
    )

  val everything =
    producer |+| consumer

  for {
    k <- everything.keys
  } yield {
    val str =
      everything.at(k).renderAs[PlantUml]

    FilePrinterAlg[IO].print(k + ".puml")(str)
      .unsafeRunSync()
  }
}
