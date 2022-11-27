package com.htmlism.temporaldiagrams.server

package object builder {
  implicit class StringKeyOps(key: String) {
    def withValue[A: ValueDecoder]: KeyValueDecoder.Value[A] =
      KeyValueDecoder.Value(List(key))

    def withRecord[A](implicit A: KeyValueDecoder.Value[A]): KeyValueDecoder.Value[A] =
      A.withNamespace(key)
  }
}
