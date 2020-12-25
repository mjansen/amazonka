{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelector
  ( DASHFragmentSelector (..),

    -- * Smart constructor
    mkDASHFragmentSelector,

    -- * Lenses
    dashfsFragmentSelectorType,
    dashfsTimestampRange,
  )
where

import qualified Network.AWS.KinesisVideoArchivedMedia.Types.DASHFragmentSelectorType as Types
import qualified Network.AWS.KinesisVideoArchivedMedia.Types.DASHTimestampRange as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the range of timestamps for the requested media, and the source of the timestamps.
--
-- /See:/ 'mkDASHFragmentSelector' smart constructor.
data DASHFragmentSelector = DASHFragmentSelector'
  { -- | The source of the timestamps for the requested media.
    --
    -- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetDASHStreamingSessionURLInput$PlaybackMode' is @ON_DEMAND@ or @LIVE_REPLAY@ , the first fragment ingested with a producer timestamp within the specified 'FragmentSelector$TimestampRange' is included in the media playlist. In addition, the fragments with producer timestamps within the @TimestampRange@ ingested immediately following the first fragment (up to the 'GetDASHStreamingSessionURLInput$MaxManifestFragmentResults' value) are included.
    -- Fragments that have duplicate producer timestamps are deduplicated. This means that if producers are producing a stream of fragments with producer timestamps that are approximately equal to the true clock time, the MPEG-DASH manifest will contain all of the fragments within the requested timestamp range. If some fragments are ingested within the same time range and very different points in time, only the oldest ingested collection of fragments are returned.
    -- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetDASHStreamingSessionURLInput$PlaybackMode' is @LIVE@ , the producer timestamps are used in the MP4 fragments and for deduplication. But the most recently ingested fragments based on server timestamps are included in the MPEG-DASH manifest. This means that even if fragments ingested in the past have producer timestamps with values now, they are not included in the HLS media playlist.
    -- The default is @SERVER_TIMESTAMP@ .
    fragmentSelectorType :: Core.Maybe Types.DASHFragmentSelectorType,
    -- | The start and end of the timestamp range for the requested media.
    --
    -- This value should not be present if @PlaybackType@ is @LIVE@ .
    timestampRange :: Core.Maybe Types.DASHTimestampRange
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DASHFragmentSelector' value with any optional fields omitted.
mkDASHFragmentSelector ::
  DASHFragmentSelector
mkDASHFragmentSelector =
  DASHFragmentSelector'
    { fragmentSelectorType = Core.Nothing,
      timestampRange = Core.Nothing
    }

-- | The source of the timestamps for the requested media.
--
-- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetDASHStreamingSessionURLInput$PlaybackMode' is @ON_DEMAND@ or @LIVE_REPLAY@ , the first fragment ingested with a producer timestamp within the specified 'FragmentSelector$TimestampRange' is included in the media playlist. In addition, the fragments with producer timestamps within the @TimestampRange@ ingested immediately following the first fragment (up to the 'GetDASHStreamingSessionURLInput$MaxManifestFragmentResults' value) are included.
-- Fragments that have duplicate producer timestamps are deduplicated. This means that if producers are producing a stream of fragments with producer timestamps that are approximately equal to the true clock time, the MPEG-DASH manifest will contain all of the fragments within the requested timestamp range. If some fragments are ingested within the same time range and very different points in time, only the oldest ingested collection of fragments are returned.
-- When @FragmentSelectorType@ is set to @PRODUCER_TIMESTAMP@ and 'GetDASHStreamingSessionURLInput$PlaybackMode' is @LIVE@ , the producer timestamps are used in the MP4 fragments and for deduplication. But the most recently ingested fragments based on server timestamps are included in the MPEG-DASH manifest. This means that even if fragments ingested in the past have producer timestamps with values now, they are not included in the HLS media playlist.
-- The default is @SERVER_TIMESTAMP@ .
--
-- /Note:/ Consider using 'fragmentSelectorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dashfsFragmentSelectorType :: Lens.Lens' DASHFragmentSelector (Core.Maybe Types.DASHFragmentSelectorType)
dashfsFragmentSelectorType = Lens.field @"fragmentSelectorType"
{-# DEPRECATED dashfsFragmentSelectorType "Use generic-lens or generic-optics with 'fragmentSelectorType' instead." #-}

-- | The start and end of the timestamp range for the requested media.
--
-- This value should not be present if @PlaybackType@ is @LIVE@ .
--
-- /Note:/ Consider using 'timestampRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dashfsTimestampRange :: Lens.Lens' DASHFragmentSelector (Core.Maybe Types.DASHTimestampRange)
dashfsTimestampRange = Lens.field @"timestampRange"
{-# DEPRECATED dashfsTimestampRange "Use generic-lens or generic-optics with 'timestampRange' instead." #-}

instance Core.FromJSON DASHFragmentSelector where
  toJSON DASHFragmentSelector {..} =
    Core.object
      ( Core.catMaybes
          [ ("FragmentSelectorType" Core..=) Core.<$> fragmentSelectorType,
            ("TimestampRange" Core..=) Core.<$> timestampRange
          ]
      )
