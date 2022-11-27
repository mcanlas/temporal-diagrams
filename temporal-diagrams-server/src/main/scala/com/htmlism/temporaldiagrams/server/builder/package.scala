package com.htmlism.temporaldiagrams.server

package object builder {
  implicit class StringKeyOps(key: String) {
    def withValue[A: ValueDecoder]: KeyValueDecoder[A] =
      KeyValueDecoder.One(key)

    def withRecord[A](implicit A: KeyValueDecoder[A]): KeyValueDecoder[A] =
      A.withNamespace(key)
  }
}
