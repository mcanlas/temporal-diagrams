package com.htmlism.temporaldiagrams.demo

import java.io.PrintWriter

import cats.effect.*

trait FilePrinter[F[_]]:
  def print(dest: String)(s: String): F[Unit]

object FilePrinter:

  def apply[F[_]](using F: Sync[F]): FilePrinter[F] =
    new FilePrinter[F]:
      def print(dest: String)(s: String): F[Unit] =
        Resource
          .fromAutoCloseable:
            F.delay:
              new PrintWriter(dest)
          .use: pw =>
            F.delay:
              pw.println(s)
