package com.htmlism.temporaldiagrams.server

import cats.data.ValidatedNec

trait RecordDecoder[A]:
  def decode(xs: Map[String, List[String]]): ValidatedNec[String, A]
