package com.htmlism.temporaldiagrams.generate

import cats.effect.*

// https://plantuml.com/api
object TestImageWriting extends IOApp.Simple:
  def run: IO[Unit] =
    val diagram =
      List(
        "@startuml",
        "Bob -> Alice : hello",
        "@enduml"
      )

    ImageWriter
      .sync[IO]
      .writeFile(
        diagram.mkString("\n"),
        "out.png"
      )
