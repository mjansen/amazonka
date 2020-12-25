{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SpliceInsertScheduleActionSettings
  ( Scte35SpliceInsertScheduleActionSettings (..),

    -- * Smart constructor
    mkScte35SpliceInsertScheduleActionSettings,

    -- * Lenses
    ssisasSpliceEventId,
    ssisasDuration,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for a SCTE-35 splice_insert message.
--
-- /See:/ 'mkScte35SpliceInsertScheduleActionSettings' smart constructor.
data Scte35SpliceInsertScheduleActionSettings = Scte35SpliceInsertScheduleActionSettings'
  { -- | The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
    spliceEventId :: Core.Natural,
    -- | Optional, the duration for the splice_insert, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. If you enter a duration, there is an expectation that the downstream system can read the duration and cue in at that time. If you do not enter a duration, the splice_insert will continue indefinitely and there is an expectation that you will enter a return_to_network to end the splice_insert at the appropriate time.
    duration :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Scte35SpliceInsertScheduleActionSettings' value with any optional fields omitted.
mkScte35SpliceInsertScheduleActionSettings ::
  -- | 'spliceEventId'
  Core.Natural ->
  Scte35SpliceInsertScheduleActionSettings
mkScte35SpliceInsertScheduleActionSettings spliceEventId =
  Scte35SpliceInsertScheduleActionSettings'
    { spliceEventId,
      duration = Core.Nothing
    }

-- | The splice_event_id for the SCTE-35 splice_insert, as defined in SCTE-35.
--
-- /Note:/ Consider using 'spliceEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssisasSpliceEventId :: Lens.Lens' Scte35SpliceInsertScheduleActionSettings Core.Natural
ssisasSpliceEventId = Lens.field @"spliceEventId"
{-# DEPRECATED ssisasSpliceEventId "Use generic-lens or generic-optics with 'spliceEventId' instead." #-}

-- | Optional, the duration for the splice_insert, in 90 KHz ticks. To convert seconds to ticks, multiple the seconds by 90,000. If you enter a duration, there is an expectation that the downstream system can read the duration and cue in at that time. If you do not enter a duration, the splice_insert will continue indefinitely and there is an expectation that you will enter a return_to_network to end the splice_insert at the appropriate time.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssisasDuration :: Lens.Lens' Scte35SpliceInsertScheduleActionSettings (Core.Maybe Core.Natural)
ssisasDuration = Lens.field @"duration"
{-# DEPRECATED ssisasDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

instance Core.FromJSON Scte35SpliceInsertScheduleActionSettings where
  toJSON Scte35SpliceInsertScheduleActionSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("spliceEventId" Core..= spliceEventId),
            ("duration" Core..=) Core.<$> duration
          ]
      )

instance Core.FromJSON Scte35SpliceInsertScheduleActionSettings where
  parseJSON =
    Core.withObject "Scte35SpliceInsertScheduleActionSettings" Core.$
      \x ->
        Scte35SpliceInsertScheduleActionSettings'
          Core.<$> (x Core..: "spliceEventId") Core.<*> (x Core..:? "duration")
