package com.htmlism.temporaldiagrams
package demo

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._

import com.htmlism.temporaldiagrams.syntax._

object Demo extends App {
  val producer =
    Temporal(
      1 -> Service("foo", None).id("foo"),
      2 -> Service("new_foo", None).r
    )

  val consumer =
    Temporal(
      1 -> Service("bar", "foo".some).id("bar"),
      2 -> Service("bar", "new_foo".some).r,
      3 -> Service("new_bar", "new_foo".some).r
    )

  val everything =
    producer |+| consumer

  for {
    k <- everything.keys
  } {
    val str =
      wrap(everything.at(k).renderAs[PlantUml])

    FilePrinterAlg[IO].print(k + ".puml")(str)
      .unsafeRunSync()
  }

  for {
    x <- List("foo", "bar")
  } {
    val str =
      wrap(everything.at(1).renderWithHighlightsOn[PlantUml](x))

    FilePrinterAlg[IO].print( x + ".puml")(str)
      .unsafeRunSync()
  }

  private def wrap(s: String) =
    List("@startuml", s, "@enduml")
      .mkString("\n")
}
