package com.htmlism.temporaldiagrams
package syntax

import cats.data.*

extension [A](x: A)
  def iff(cond: Boolean, f: A => A): A =
    if cond then f(x)
    else x

  def list: List[A] =
    List(x)

  def nel: NonEmptyList[A] =
    NonEmptyList.one(x)
