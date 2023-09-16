package com.htmlism.temporaldiagrams.demo.v2

sealed trait DemoDsl

object DemoDsl {
  case class Service(name: String, dependency: Option[String])  extends DemoDsl
  case class Hydra(name: String, dependency: Option[String])    extends DemoDsl
  case class Buffered(name: String, dependency: Option[String]) extends DemoDsl
}
