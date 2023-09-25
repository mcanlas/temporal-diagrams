package com.htmlism.temporaldiagrams.demo.v2

sealed trait OuroborosDsl

object OuroborosDsl {
  case class Type(name: String) extends OuroborosDsl

  case class Encoding(src: String, dest: String) extends OuroborosDsl
}
