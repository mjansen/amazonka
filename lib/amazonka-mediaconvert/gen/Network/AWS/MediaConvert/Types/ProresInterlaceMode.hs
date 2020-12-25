{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ProresInterlaceMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ProresInterlaceMode
  ( ProresInterlaceMode
      ( ProresInterlaceMode',
        ProresInterlaceModeProgressive,
        ProresInterlaceModeTopField,
        ProresInterlaceModeBottomField,
        ProresInterlaceModeFollowTopField,
        ProresInterlaceModeFollowBottomField,
        fromProresInterlaceMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Choose the scan line type for the output. Keep the default value, Progressive (PROGRESSIVE) to create a progressive output, regardless of the scan type of your input. Use Top field first (TOP_FIELD) or Bottom field first (BOTTOM_FIELD) to create an output that's interlaced with the same field polarity throughout. Use Follow, default top (FOLLOW_TOP_FIELD) or Follow, default bottom (FOLLOW_BOTTOM_FIELD) to produce outputs with the same field polarity as the source. For jobs that have multiple inputs, the output field polarity might change over the course of the output. Follow behavior depends on the input scan type. If the source is interlaced, the output will be interlaced with the same polarity as the source. If the source is progressive, the output will be interlaced with top field bottom field first, depending on which of the Follow options you choose.
newtype ProresInterlaceMode = ProresInterlaceMode'
  { fromProresInterlaceMode ::
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

pattern ProresInterlaceModeProgressive :: ProresInterlaceMode
pattern ProresInterlaceModeProgressive = ProresInterlaceMode' "PROGRESSIVE"

pattern ProresInterlaceModeTopField :: ProresInterlaceMode
pattern ProresInterlaceModeTopField = ProresInterlaceMode' "TOP_FIELD"

pattern ProresInterlaceModeBottomField :: ProresInterlaceMode
pattern ProresInterlaceModeBottomField = ProresInterlaceMode' "BOTTOM_FIELD"

pattern ProresInterlaceModeFollowTopField :: ProresInterlaceMode
pattern ProresInterlaceModeFollowTopField = ProresInterlaceMode' "FOLLOW_TOP_FIELD"

pattern ProresInterlaceModeFollowBottomField :: ProresInterlaceMode
pattern ProresInterlaceModeFollowBottomField = ProresInterlaceMode' "FOLLOW_BOTTOM_FIELD"

{-# COMPLETE
  ProresInterlaceModeProgressive,
  ProresInterlaceModeTopField,
  ProresInterlaceModeBottomField,
  ProresInterlaceModeFollowTopField,
  ProresInterlaceModeFollowBottomField,
  ProresInterlaceMode'
  #-}
