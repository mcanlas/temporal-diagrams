package com.htmlism.temporaldiagrams

/**
  * There exists multiple dialects, of which PlantUml is one
  */
trait Dialect[A] {
  def joiner: String
}
