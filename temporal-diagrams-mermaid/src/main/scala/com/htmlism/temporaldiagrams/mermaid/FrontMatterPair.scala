package com.htmlism.temporaldiagrams.mermaid

import cats.data.Chain

enum FrontMatterPair:
  case StringPair(key: String, value: String)
  case MapPair(key: String, xs: Chain[FrontMatterPair])

object FrontMatterPair:
  object StringPair:
    def from[A, B](a: A, b: B): StringPair =
      StringPair(a.toString, b.toString)

  object MapPair:
    def from[A](a: A, xs: Chain[FrontMatterPair]): MapPair =
      MapPair(a.toString, xs)

  def encode(x: FrontMatterPair): Chain[String] =
    x match
      case StringPair(k, v) =>
        Chain.one:
          s"$k: $v"

      case MapPair(k, xs) =>
        val firstLine =
          Chain.one:
            s"$k:"

        val body =
          xs
            .flatMap(encode)
            .map("  " + _)

        firstLine ++ body
