package com.htmlism.temporaldiagrams.server
package builder

extension (key: String)
  def as[A: ValueDecoder]: KeyValuePairsDecoder[A] =
    KeyValuePairsDecoder.One(key)

  def namespaces[A](using A: KeyValuePairsDecoder[A]): KeyValuePairsDecoder[A] =
    A.withNamespace(key)
