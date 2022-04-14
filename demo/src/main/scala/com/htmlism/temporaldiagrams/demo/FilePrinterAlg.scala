package com.htmlism.temporaldiagrams.demo

import java.io.PrintWriter

import cats.effect._

trait FilePrinterAlg[F[_]] {
  def print(dest: String)(s: String): F[Unit]
}

object FilePrinterAlg {

  def apply[F[_]](implicit F: Sync[F]): FilePrinterAlg[F] =
    new FilePrinterAlg[F] {

      def print(dest: String)(s: String): F[Unit] =
        Resource
          .fromAutoCloseable {
            F.delay {
              new PrintWriter(dest)
            }
          }
          .use { pw =>
            F.delay {
              pw.println(s)
            }
          }
    }
}
