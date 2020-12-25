{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Vp9QualityTuningLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Vp9QualityTuningLevel
  ( Vp9QualityTuningLevel
      ( Vp9QualityTuningLevel',
        Vp9QualityTuningLevelMultiPass,
        Vp9QualityTuningLevelMultiPassHq,
        fromVp9QualityTuningLevel
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Optional. Use Quality tuning level (qualityTuningLevel) to choose how you want to trade off encoding speed for output video quality. The default behavior is faster, lower quality, multi-pass encoding.
newtype Vp9QualityTuningLevel = Vp9QualityTuningLevel'
  { fromVp9QualityTuningLevel ::
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

pattern Vp9QualityTuningLevelMultiPass :: Vp9QualityTuningLevel
pattern Vp9QualityTuningLevelMultiPass = Vp9QualityTuningLevel' "MULTI_PASS"

pattern Vp9QualityTuningLevelMultiPassHq :: Vp9QualityTuningLevel
pattern Vp9QualityTuningLevelMultiPassHq = Vp9QualityTuningLevel' "MULTI_PASS_HQ"

{-# COMPLETE
  Vp9QualityTuningLevelMultiPass,
  Vp9QualityTuningLevelMultiPassHq,
  Vp9QualityTuningLevel'
  #-}
