package com.htmlism

import cats.data._

package object temporaldiagrams {
  type Nel[+A] =
    NonEmptyList[A]

  val Nel: NonEmptyList.type =
    NonEmptyList
}
