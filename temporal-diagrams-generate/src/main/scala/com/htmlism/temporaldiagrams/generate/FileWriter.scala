package com.htmlism.temporaldiagrams.generate

import java.nio.file.*

import scala.jdk.CollectionConverters.*

import cats.effect.*
import cats.syntax.all.*

trait FileWriter[F[_]]:
  def writeLines(dest: String, xs: Iterable[String]): F[Unit]

object FileWriter:
  def sync[F[_]](using F: Sync[F]): FileWriter[F] =
    new FileWriter[F]:
      def writeLines(dest: String, xs: Iterable[String]): F[Unit] =
        F.blocking:
          Files
            .write(Path.of(dest), xs.asJava)
        .void
