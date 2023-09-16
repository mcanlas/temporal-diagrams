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
      * Marks an expression as renderable to `D`, without any tags
      *
      * @tparam D
      *   The target diagram language
      */
    def r[D](implicit enc: HighlightEncoder[D, A]): Renderable[D] =
      RenderableA(x, Nil)

    /**
      * Marks an expression as renderable to `D`, with the specified tags
      *
      * @param t
      *   A required tag
      * @param ts
      *   Optional, additional tags
      * @tparam D
      *   The target diagram language
      */

    def tag[D](t: String, ts: String*)(implicit enc: HighlightEncoder[D, A]): Renderable[D] =
      RenderableA(x, t :: ts.toList)
  }
}
