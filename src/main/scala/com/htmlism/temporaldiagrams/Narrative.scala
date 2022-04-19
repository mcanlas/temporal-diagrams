package com.htmlism.temporaldiagrams

import com.htmlism.temporaldiagrams.FacetedFrame.FrameId

case class Narrative[K, A](frames: Nel[FacetedFrame[K, A]], episodeSelectors: Nel[List[(FrameId, K)]]) {
  def next(selectors: (FrameId, K)*): Narrative[K, A] = {
    val previously =
      episodeSelectors.last

    val nextEpisodeSelectors =
      previously concat selectors

    copy(episodeSelectors = episodeSelectors.append(nextEpisodeSelectors))
  }

  def reset(selectors: (FrameId, K)*): Narrative[K, A] = {
    copy(episodeSelectors = episodeSelectors.append(selectors.toList))
  }

  def episodes: Nel[Nel[Renderable[A]]] =
    episodeSelectors
      .map(FacetedFrame.selectFrames(frames, _: _*))
}
