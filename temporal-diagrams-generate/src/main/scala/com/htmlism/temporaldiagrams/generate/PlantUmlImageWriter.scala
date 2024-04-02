package com.htmlism.temporaldiagrams.generate

import java.io.FileOutputStream

import cats.effect.*
import cats.syntax.all.*
import net.sourceforge.plantuml.SourceStringReader

trait PlantUmlImageWriter[F[_]]:
  def writeFile(body: String, destination: String): F[Unit]

object PlantUmlImageWriter:
  private def fileOutputStream[F[_]: Sync](dest: String): Resource[F, FileOutputStream] =
    Resource
      .fromAutoCloseable:
        Sync[F].blocking:
          FileOutputStream(dest)

  def sync[F[_]: Sync](using out: std.Console[F]): PlantUmlImageWriter[F] =
    new PlantUmlImageWriter[F]:
      def writeFile(body: String, destination: String): F[Unit] =
        fileOutputStream[F](destination)
          .use: os =>
            for
              desc <- Sync[F].blocking:
                val reader = SourceStringReader(body)

                reader.outputImage(os)

              _ <- out.println(s"Wrote to $destination : $desc")
            yield ()
