package com.htmlism.temporaldiagrams.server

package object builder {
  implicit class StringKeyOps(key: String) {
    def as[A: ValueDecoder]: KeyValuePairsDecoder[A] =
      KeyValuePairsDecoder.One(key)

    def namespaces[A](implicit A: KeyValuePairsDecoder[A]): KeyValuePairsDecoder[A] =
      A.withNamespace(key)
  }
}
