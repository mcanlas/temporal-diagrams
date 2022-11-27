package com.htmlism.temporaldiagrams.server

import cats._
import cats.data.ValidatedNec

trait QueryStringDecoder[A] {
  def decode(queryString: Map[String, List[String]]): ValidatedNec[String, A]
}

object QueryStringDecoder {
  implicit val functorQueryStringDecoder: Functor[QueryStringDecoder] =
    new Functor[QueryStringDecoder] {
      def map[A, B](fa: QueryStringDecoder[A])(f: A => B): QueryStringDecoder[B] =
        (queryString: Map[String, List[String]]) => fa.decode(queryString).map(f)
    }
}
