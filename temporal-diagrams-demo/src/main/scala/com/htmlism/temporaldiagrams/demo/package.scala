package com.htmlism.temporaldiagrams.demo

extension [A](x: A)
  /**
    * Allow an optional value to influence some expression, in postfix syntax
    *
    * @param ob
    *   A secondary value that may or may not be available
    * @param f
    *   A function to produce a new value given the original expression and the secondary value
    * @tparam AA
    *   A potentially new supertype for `A`
    * @tparam B
    *   The type of the optional payload
    */
  def applySome[AA >: A, B](ob: Option[B])(f: (A, B) => AA): AA =
    ob
      .map(f(x, _))
      .getOrElse(x)
