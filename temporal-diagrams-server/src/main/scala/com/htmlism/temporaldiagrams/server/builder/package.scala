package com.htmlism.temporaldiagrams.server

package object builder {
  implicit class StringKeyOps(key: String) {
    def asValue[A: ValueDecoder]: KeyValuePairsDecoder[A] =
      KeyValuePairsDecoder.One(key)

    def asRecord[A](implicit A: KeyValuePairsDecoder[A]): KeyValuePairsDecoder[A] =
      A.withNamespace(key)
  }
}
