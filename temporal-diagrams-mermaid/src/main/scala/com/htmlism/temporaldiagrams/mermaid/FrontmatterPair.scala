package com.htmlism.temporaldiagrams.mermaid

import cats.data.Chain

enum FrontmatterPair:
  case StringPair(key: String, value: String)
  case MapPair(key: String, xs: Chain[FrontmatterPair])

object FrontmatterPair:
  def encode(x: FrontmatterPair): Chain[String] =
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
