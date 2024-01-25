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

  def r: Renderable.Tagged[A] =
    Renderable.Tagged(Nil, x)

  def tag(tags: String*): Renderable.Tagged[A] =
    Renderable.Tagged(tags.toList, x)

// TODO get rid of this, and all faceted frames
extension [A](x: Renderable.Tagged[A])
  def f[K]: FacetedFrame[K, A] =
    FacetedFrame.fixed(x.list)
