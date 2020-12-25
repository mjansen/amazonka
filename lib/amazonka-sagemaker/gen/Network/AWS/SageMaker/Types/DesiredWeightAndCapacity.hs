{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DesiredWeightAndCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DesiredWeightAndCapacity
  ( DesiredWeightAndCapacity (..),

    -- * Smart constructor
    mkDesiredWeightAndCapacity,

    -- * Lenses
    dwacVariantName,
    dwacDesiredInstanceCount,
    dwacDesiredWeight,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.VariantName as Types

-- | Specifies weight and capacity values for a production variant.
--
-- /See:/ 'mkDesiredWeightAndCapacity' smart constructor.
data DesiredWeightAndCapacity = DesiredWeightAndCapacity'
  { -- | The name of the variant to update.
    variantName :: Types.VariantName,
    -- | The variant's capacity.
    desiredInstanceCount :: Core.Maybe Core.Natural,
    -- | The variant's weight.
    desiredWeight :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DesiredWeightAndCapacity' value with any optional fields omitted.
mkDesiredWeightAndCapacity ::
  -- | 'variantName'
  Types.VariantName ->
  DesiredWeightAndCapacity
mkDesiredWeightAndCapacity variantName =
  DesiredWeightAndCapacity'
    { variantName,
      desiredInstanceCount = Core.Nothing,
      desiredWeight = Core.Nothing
    }

-- | The name of the variant to update.
--
-- /Note:/ Consider using 'variantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwacVariantName :: Lens.Lens' DesiredWeightAndCapacity Types.VariantName
dwacVariantName = Lens.field @"variantName"
{-# DEPRECATED dwacVariantName "Use generic-lens or generic-optics with 'variantName' instead." #-}

-- | The variant's capacity.
--
-- /Note:/ Consider using 'desiredInstanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwacDesiredInstanceCount :: Lens.Lens' DesiredWeightAndCapacity (Core.Maybe Core.Natural)
dwacDesiredInstanceCount = Lens.field @"desiredInstanceCount"
{-# DEPRECATED dwacDesiredInstanceCount "Use generic-lens or generic-optics with 'desiredInstanceCount' instead." #-}

-- | The variant's weight.
--
-- /Note:/ Consider using 'desiredWeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwacDesiredWeight :: Lens.Lens' DesiredWeightAndCapacity (Core.Maybe Core.Double)
dwacDesiredWeight = Lens.field @"desiredWeight"
{-# DEPRECATED dwacDesiredWeight "Use generic-lens or generic-optics with 'desiredWeight' instead." #-}

instance Core.FromJSON DesiredWeightAndCapacity where
  toJSON DesiredWeightAndCapacity {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("VariantName" Core..= variantName),
            ("DesiredInstanceCount" Core..=) Core.<$> desiredInstanceCount,
            ("DesiredWeight" Core..=) Core.<$> desiredWeight
          ]
      )
