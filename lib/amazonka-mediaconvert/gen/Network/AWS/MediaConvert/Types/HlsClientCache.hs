{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HlsClientCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsClientCache
  ( HlsClientCache
      ( HlsClientCache',
        HlsClientCacheDisabled,
        HlsClientCacheEnabled,
        fromHlsClientCache
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Disable this setting only when your workflow requires the #EXT-X-ALLOW-CACHE:no tag. Otherwise, keep the default value Enabled (ENABLED) and control caching in your video distribution set up. For example, use the Cache-Control http header.
newtype HlsClientCache = HlsClientCache'
  { fromHlsClientCache ::
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

pattern HlsClientCacheDisabled :: HlsClientCache
pattern HlsClientCacheDisabled = HlsClientCache' "DISABLED"

pattern HlsClientCacheEnabled :: HlsClientCache
pattern HlsClientCacheEnabled = HlsClientCache' "ENABLED"

{-# COMPLETE
  HlsClientCacheDisabled,
  HlsClientCacheEnabled,
  HlsClientCache'
  #-}
