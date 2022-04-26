package com.htmlism.temporaldiagrams

import cats.data._

case class Narrative[K, A](
    frames: NonEmptyList[FacetedFrame[K, A]],
    episodeSelectors: NonEmptyList[List[(String, K)]]
) {
  def next(selectors: (String, K)*): Narrative[K, A] = {
    val previously =
      episodeSelectors.last

    // since first selector wins, we want the new ones to go first
    // e.g. selecting the next variant of the same component/frame
    val nextEpisodeSelectors =
      selectors.toList ::: previously

    copy(episodeSelectors = episodeSelectors.append(nextEpisodeSelectors))
  }

  def reset(selectors: (String, K)*): Narrative[K, A] = {
    copy(episodeSelectors = episodeSelectors.append(selectors.toList))
  }

  def episodes: NonEmptyList[List[Renderable[A]]] =
    episodeSelectors
      .map(FacetedFrame.selectFrames(frames, _: _*))
}
