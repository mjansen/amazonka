{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesConfiguration
  ( ReservedInstancesConfiguration (..),

    -- * Smart constructor
    mkReservedInstancesConfiguration,

    -- * Lenses
    ricAvailabilityZone,
    ricInstanceCount,
    ricInstanceType,
    ricPlatform,
    ricScope,
  )
where

import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.Scope as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the configuration settings for the modified Reserved Instances.
--
-- /See:/ 'mkReservedInstancesConfiguration' smart constructor.
data ReservedInstancesConfiguration = ReservedInstancesConfiguration'
  { -- | The Availability Zone for the modified Reserved Instances.
    availabilityZone :: Core.Maybe Types.String,
    -- | The number of modified Reserved Instances.
    instanceCount :: Core.Maybe Core.Int,
    -- | The instance type for the modified Reserved Instances.
    instanceType :: Core.Maybe Types.InstanceType,
    -- | The network platform of the modified Reserved Instances, which is either EC2-Classic or EC2-VPC.
    platform :: Core.Maybe Types.String,
    -- | Whether the Reserved Instance is applied to instances in a Region or instances in a specific Availability Zone.
    scope :: Core.Maybe Types.Scope
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservedInstancesConfiguration' value with any optional fields omitted.
mkReservedInstancesConfiguration ::
  ReservedInstancesConfiguration
mkReservedInstancesConfiguration =
  ReservedInstancesConfiguration'
    { availabilityZone = Core.Nothing,
      instanceCount = Core.Nothing,
      instanceType = Core.Nothing,
      platform = Core.Nothing,
      scope = Core.Nothing
    }

-- | The Availability Zone for the modified Reserved Instances.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ricAvailabilityZone :: Lens.Lens' ReservedInstancesConfiguration (Core.Maybe Types.String)
ricAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED ricAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The number of modified Reserved Instances.
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ricInstanceCount :: Lens.Lens' ReservedInstancesConfiguration (Core.Maybe Core.Int)
ricInstanceCount = Lens.field @"instanceCount"
{-# DEPRECATED ricInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The instance type for the modified Reserved Instances.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ricInstanceType :: Lens.Lens' ReservedInstancesConfiguration (Core.Maybe Types.InstanceType)
ricInstanceType = Lens.field @"instanceType"
{-# DEPRECATED ricInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The network platform of the modified Reserved Instances, which is either EC2-Classic or EC2-VPC.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ricPlatform :: Lens.Lens' ReservedInstancesConfiguration (Core.Maybe Types.String)
ricPlatform = Lens.field @"platform"
{-# DEPRECATED ricPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | Whether the Reserved Instance is applied to instances in a Region or instances in a specific Availability Zone.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ricScope :: Lens.Lens' ReservedInstancesConfiguration (Core.Maybe Types.Scope)
ricScope = Lens.field @"scope"
{-# DEPRECATED ricScope "Use generic-lens or generic-optics with 'scope' instead." #-}

instance Core.FromXML ReservedInstancesConfiguration where
  parseXML x =
    ReservedInstancesConfiguration'
      Core.<$> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "instanceCount")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "platform")
      Core.<*> (x Core..@? "scope")
