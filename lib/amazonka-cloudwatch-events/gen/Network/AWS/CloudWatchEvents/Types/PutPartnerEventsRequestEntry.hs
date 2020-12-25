{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.PutPartnerEventsRequestEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.PutPartnerEventsRequestEntry
  ( PutPartnerEventsRequestEntry (..),

    -- * Smart constructor
    mkPutPartnerEventsRequestEntry,

    -- * Lenses
    ppereDetail,
    ppereDetailType,
    ppereResources,
    ppereSource,
    ppereTime,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types.EventResource as Types
import qualified Network.AWS.CloudWatchEvents.Types.Source as Types
import qualified Network.AWS.CloudWatchEvents.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details about an event generated by an SaaS partner.
--
-- /See:/ 'mkPutPartnerEventsRequestEntry' smart constructor.
data PutPartnerEventsRequestEntry = PutPartnerEventsRequestEntry'
  { -- | A valid JSON string. There is no other schema imposed. The JSON string may contain fields and nested subobjects.
    detail :: Core.Maybe Types.String,
    -- | A free-form string used to decide what fields to expect in the event detail.
    detailType :: Core.Maybe Types.String,
    -- | AWS resources, identified by Amazon Resource Name (ARN), which the event primarily concerns. Any number, including zero, may be present.
    resources :: Core.Maybe [Types.EventResource],
    -- | The event source that is generating the evntry.
    source :: Core.Maybe Types.Source,
    -- | The date and time of the event.
    time :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutPartnerEventsRequestEntry' value with any optional fields omitted.
mkPutPartnerEventsRequestEntry ::
  PutPartnerEventsRequestEntry
mkPutPartnerEventsRequestEntry =
  PutPartnerEventsRequestEntry'
    { detail = Core.Nothing,
      detailType = Core.Nothing,
      resources = Core.Nothing,
      source = Core.Nothing,
      time = Core.Nothing
    }

-- | A valid JSON string. There is no other schema imposed. The JSON string may contain fields and nested subobjects.
--
-- /Note:/ Consider using 'detail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppereDetail :: Lens.Lens' PutPartnerEventsRequestEntry (Core.Maybe Types.String)
ppereDetail = Lens.field @"detail"
{-# DEPRECATED ppereDetail "Use generic-lens or generic-optics with 'detail' instead." #-}

-- | A free-form string used to decide what fields to expect in the event detail.
--
-- /Note:/ Consider using 'detailType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppereDetailType :: Lens.Lens' PutPartnerEventsRequestEntry (Core.Maybe Types.String)
ppereDetailType = Lens.field @"detailType"
{-# DEPRECATED ppereDetailType "Use generic-lens or generic-optics with 'detailType' instead." #-}

-- | AWS resources, identified by Amazon Resource Name (ARN), which the event primarily concerns. Any number, including zero, may be present.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppereResources :: Lens.Lens' PutPartnerEventsRequestEntry (Core.Maybe [Types.EventResource])
ppereResources = Lens.field @"resources"
{-# DEPRECATED ppereResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | The event source that is generating the evntry.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppereSource :: Lens.Lens' PutPartnerEventsRequestEntry (Core.Maybe Types.Source)
ppereSource = Lens.field @"source"
{-# DEPRECATED ppereSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The date and time of the event.
--
-- /Note:/ Consider using 'time' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppereTime :: Lens.Lens' PutPartnerEventsRequestEntry (Core.Maybe Core.NominalDiffTime)
ppereTime = Lens.field @"time"
{-# DEPRECATED ppereTime "Use generic-lens or generic-optics with 'time' instead." #-}

instance Core.FromJSON PutPartnerEventsRequestEntry where
  toJSON PutPartnerEventsRequestEntry {..} =
    Core.object
      ( Core.catMaybes
          [ ("Detail" Core..=) Core.<$> detail,
            ("DetailType" Core..=) Core.<$> detailType,
            ("Resources" Core..=) Core.<$> resources,
            ("Source" Core..=) Core.<$> source,
            ("Time" Core..=) Core.<$> time
          ]
      )
