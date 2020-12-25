{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsAudioDuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsAudioDuration
  ( M2tsAudioDuration
      ( M2tsAudioDuration',
        M2tsAudioDurationDefaultCodecDuration,
        M2tsAudioDurationMatchVideoDuration,
        fromM2tsAudioDuration
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify this setting only when your output will be consumed by a downstream repackaging workflow that is sensitive to very small duration differences between video and audio. For this situation, choose Match video duration (MATCH_VIDEO_DURATION). In all other cases, keep the default value, Default codec duration (DEFAULT_CODEC_DURATION). When you choose Match video duration, MediaConvert pads the output audio streams with silence or trims them to ensure that the total duration of each audio stream is at least as long as the total duration of the video stream. After padding or trimming, the audio stream duration is no more than one frame longer than the video stream. MediaConvert applies audio padding or trimming only to the end of the last segment of the output. For unsegmented outputs, MediaConvert adds padding only to the end of the file. When you keep the default value, any minor discrepancies between audio and video duration will depend on your output audio codec.
newtype M2tsAudioDuration = M2tsAudioDuration'
  { fromM2tsAudioDuration ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern M2tsAudioDurationDefaultCodecDuration :: M2tsAudioDuration
pattern M2tsAudioDurationDefaultCodecDuration = M2tsAudioDuration' "DEFAULT_CODEC_DURATION"

pattern M2tsAudioDurationMatchVideoDuration :: M2tsAudioDuration
pattern M2tsAudioDurationMatchVideoDuration = M2tsAudioDuration' "MATCH_VIDEO_DURATION"

{-# COMPLETE
  M2tsAudioDurationDefaultCodecDuration,
  M2tsAudioDurationMatchVideoDuration,
  M2tsAudioDuration'
  #-}
