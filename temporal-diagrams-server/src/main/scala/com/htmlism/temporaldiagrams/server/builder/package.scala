package com.htmlism.temporaldiagrams.server

package object builder {
  implicit class StringKeyOps(key: String) {
    def withValue[A: ValueDecoder]: KeyValueDecoder[A] =
      KeyValueDecoder.Value(key)

    def withRecord[A: RecordDecoder]: KeyValueDecoder[A] =
      KeyValueDecoder.Record(key)
  }
}
