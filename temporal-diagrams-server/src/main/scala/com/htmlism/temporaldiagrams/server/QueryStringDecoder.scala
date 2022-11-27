package com.htmlism.temporaldiagrams.server

import cats.data._

/**
  * A type class to decode a query string into a structure, or return an accumulated structure of errors
  */
trait QueryStringDecoder[A] {
  def decode(queryString: Map[String, List[String]]): ValidatedNec[String, A]
}

object QueryStringDecoder {
  def apply[A](implicit ev: KeyValuePairsDecoder[A]): QueryStringDecoder[A] =
    (queryString: Map[String, List[String]]) => ev.decode(queryString, Chain.empty)
}
