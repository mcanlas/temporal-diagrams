package com.htmlism.temporaldiagrams.v2

import scala.collection.immutable.ListSet

case class Tagged[A](x: A, tags: ListSet[String])
