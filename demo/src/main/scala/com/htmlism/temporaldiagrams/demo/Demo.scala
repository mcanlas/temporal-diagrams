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
      1 -> Service("bar", Service("foo", None).some).id("bar"),
      2 -> Service("bar", Service("new_foo", None).some).r,
      3 -> Service("new_bar", Service("new_foo", None).some).r
    )

  val everything =
    producer |+| consumer

  for {
    k <- everything.keys
  } {
    val str =
      "@startuml\n" +
      everything.at(k).renderAs[PlantUml] +
        "\n@enduml\n"

    FilePrinterAlg[IO].print(k + ".puml")(str)
      .unsafeRunSync()
  }

  for {
    x <- List("foo", "bar")
  } {
    val str =
      "@startuml\n" +
        everything.at(1).renderWithHighlightsOn[PlantUml](x) +
        "\n@enduml\n"

    FilePrinterAlg[IO].print( x + ".puml")(str)
      .unsafeRunSync()
  }


}
