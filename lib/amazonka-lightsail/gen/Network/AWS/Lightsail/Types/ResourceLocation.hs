{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ResourceLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ResourceLocation
  ( ResourceLocation (..),

    -- * Smart constructor
    mkResourceLocation,

    -- * Lenses
    rlAvailabilityZone,
    rlRegionName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.RegionName as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the resource location.
--
-- /See:/ 'mkResourceLocation' smart constructor.
data ResourceLocation = ResourceLocation'
  { -- | The Availability Zone. Follows the format @us-east-2a@ (case-sensitive).
    availabilityZone :: Core.Maybe Types.String,
    -- | The AWS Region name.
    regionName :: Core.Maybe Types.RegionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourceLocation' value with any optional fields omitted.
mkResourceLocation ::
  ResourceLocation
mkResourceLocation =
  ResourceLocation'
    { availabilityZone = Core.Nothing,
      regionName = Core.Nothing
    }

-- | The Availability Zone. Follows the format @us-east-2a@ (case-sensitive).
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlAvailabilityZone :: Lens.Lens' ResourceLocation (Core.Maybe Types.String)
rlAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED rlAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The AWS Region name.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlRegionName :: Lens.Lens' ResourceLocation (Core.Maybe Types.RegionName)
rlRegionName = Lens.field @"regionName"
{-# DEPRECATED rlRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Core.FromJSON ResourceLocation where
  parseJSON =
    Core.withObject "ResourceLocation" Core.$
      \x ->
        ResourceLocation'
          Core.<$> (x Core..:? "availabilityZone") Core.<*> (x Core..:? "regionName")
