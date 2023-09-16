package com.htmlism.temporaldiagrams.v2

package object syntax {

  /**
    * Postfix syntax enhancement for marking an expression as renderable
    *
    * @param x
    *   The data being rendered
    * @tparam A
    *   The type of the data being rendered
    */
  implicit class RenderableOps[A](x: A) {

    /**
      * Marks the expression as renderable to `D`, without any tags
      *
      * @tparam D
      *   The target diagram language
      */
    def r[D](implicit enc: HighlightEncoder[D, A]): Renderable[D] =
      RenderableA(x, Nil)
  }
}
