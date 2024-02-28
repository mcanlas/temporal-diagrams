package com.htmlism.temporaldiagrams.mermaid

import cats.data.Chain

enum FrontmatterPair:
  case StringPair(key: String, value: String)
  case MapPair(key: String, xs: List[FrontmatterPair])

object FrontmatterPair:
  def encode(x: FrontmatterPair): Chain[String] =
    x match
      case StringPair(k, v) =>
        Chain.one:
          s"$k: $v"

      case MapPair(k, _) =>
        Chain.one:
          s"$k: $k"
