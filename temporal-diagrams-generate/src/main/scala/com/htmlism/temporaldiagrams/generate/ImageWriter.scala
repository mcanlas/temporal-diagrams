package com.htmlism.temporaldiagrams.generate

import java.io.FileOutputStream

import cats.effect.*
import cats.syntax.all.*
import net.sourceforge.plantuml.SourceStringReader

trait ImageWriter[F[_]]:
  def writeFile(body: String, destination: String): F[Unit]

object ImageWriter:
  private def fileOutputStream[F[_]: Sync](dest: String): Resource[F, FileOutputStream] =
    Resource
      .fromAutoCloseable:
        Sync[F].delay:
          FileOutputStream(dest)

  def sync[F[_]: Sync](using out: std.Console[F]): ImageWriter[F] =
    new ImageWriter[F]:
      def writeFile(body: String, destination: String): F[Unit] =
        fileOutputStream[F](destination)
          .use: os =>
            val reader = SourceStringReader(body)

            for
              desc <- Sync[F].blocking:
                reader.outputImage(os)

              _ <- out.println(s"Wrote to $destination : $desc")
            yield ()
