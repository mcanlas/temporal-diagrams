package com.htmlism.temporaldiagrams

/**
  * A type class that describes encoding a multi arrow specification into a target domain language
  *
  * @tparam A
  *   The target domain language being encoded to
  */
trait MultiArrowEncoder[K, A]:
  def encodeArrow(source: K, destination: K): A
