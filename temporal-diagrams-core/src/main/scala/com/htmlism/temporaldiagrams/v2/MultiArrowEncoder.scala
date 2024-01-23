package com.htmlism.temporaldiagrams.v2

/**
  * A type class that describes encoding a multi arrow specification into a target domain language
  *
  * @tparam A
  *   The target domain language being encoded to
  */
trait MultiArrowEncoder[A]:
  def encodeArrow(source: String, destination: String): A
