{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.Types.InsightSelector
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.InsightSelector
  ( InsightSelector (..),

    -- * Smart constructor
    mkInsightSelector,

    -- * Lenses
    isInsightType,
  )
where

import qualified Network.AWS.CloudTrail.Types.InsightType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A JSON string that contains a list of insight types that are logged on a trail.
--
-- /See:/ 'mkInsightSelector' smart constructor.
newtype InsightSelector = InsightSelector'
  { -- | The type of insights to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
    insightType :: Core.Maybe Types.InsightType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'InsightSelector' value with any optional fields omitted.
mkInsightSelector ::
  InsightSelector
mkInsightSelector = InsightSelector' {insightType = Core.Nothing}

-- | The type of insights to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
--
-- /Note:/ Consider using 'insightType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isInsightType :: Lens.Lens' InsightSelector (Core.Maybe Types.InsightType)
isInsightType = Lens.field @"insightType"
{-# DEPRECATED isInsightType "Use generic-lens or generic-optics with 'insightType' instead." #-}

instance Core.FromJSON InsightSelector where
  toJSON InsightSelector {..} =
    Core.object
      (Core.catMaybes [("InsightType" Core..=) Core.<$> insightType])

instance Core.FromJSON InsightSelector where
  parseJSON =
    Core.withObject "InsightSelector" Core.$
      \x -> InsightSelector' Core.<$> (x Core..:? "InsightType")
