{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.Types.AutoScalingInstanceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.AutoScalingInstanceDetails
  ( AutoScalingInstanceDetails (..),

    -- * Smart constructor
    mkAutoScalingInstanceDetails,

    -- * Lenses
    asidInstanceId,
    asidAutoScalingGroupName,
    asidAvailabilityZone,
    asidLifecycleState,
    asidHealthStatus,
    asidProtectedFromScaleIn,
    asidInstanceType,
    asidLaunchConfigurationName,
    asidLaunchTemplate,
    asidWeightedCapacity,
  )
where

import qualified Network.AWS.AutoScaling.Types.LaunchTemplateSpecification as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen19 as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen255 as Types
import qualified Network.AWS.AutoScaling.Types.XmlStringMaxLen32 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an EC2 instance associated with an Auto Scaling group.
--
-- /See:/ 'mkAutoScalingInstanceDetails' smart constructor.
data AutoScalingInstanceDetails = AutoScalingInstanceDetails'
  { -- | The ID of the instance.
    instanceId :: Types.XmlStringMaxLen19,
    -- | The name of the Auto Scaling group for the instance.
    autoScalingGroupName :: Types.XmlStringMaxLen255,
    -- | The Availability Zone for the instance.
    availabilityZone :: Types.XmlStringMaxLen255,
    -- | The lifecycle state for the instance.
    lifecycleState :: Types.XmlStringMaxLen32,
    -- | The last reported health status of this instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Amazon EC2 Auto Scaling should terminate and replace it.
    healthStatus :: Types.XmlStringMaxLen32,
    -- | Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
    protectedFromScaleIn :: Core.Bool,
    -- | The instance type of the EC2 instance.
    instanceType :: Core.Maybe Types.XmlStringMaxLen255,
    -- | The launch configuration used to launch the instance. This value is not available if you attached the instance to the Auto Scaling group.
    launchConfigurationName :: Core.Maybe Types.XmlStringMaxLen255,
    -- | The launch template for the instance.
    launchTemplate :: Core.Maybe Types.LaunchTemplateSpecification,
    -- | The number of capacity units contributed by the instance based on its instance type.
    --
    -- Valid Range: Minimum value of 1. Maximum value of 999.
    weightedCapacity :: Core.Maybe Types.XmlStringMaxLen32
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingInstanceDetails' value with any optional fields omitted.
mkAutoScalingInstanceDetails ::
  -- | 'instanceId'
  Types.XmlStringMaxLen19 ->
  -- | 'autoScalingGroupName'
  Types.XmlStringMaxLen255 ->
  -- | 'availabilityZone'
  Types.XmlStringMaxLen255 ->
  -- | 'lifecycleState'
  Types.XmlStringMaxLen32 ->
  -- | 'healthStatus'
  Types.XmlStringMaxLen32 ->
  -- | 'protectedFromScaleIn'
  Core.Bool ->
  AutoScalingInstanceDetails
mkAutoScalingInstanceDetails
  instanceId
  autoScalingGroupName
  availabilityZone
  lifecycleState
  healthStatus
  protectedFromScaleIn =
    AutoScalingInstanceDetails'
      { instanceId,
        autoScalingGroupName,
        availabilityZone,
        lifecycleState,
        healthStatus,
        protectedFromScaleIn,
        instanceType = Core.Nothing,
        launchConfigurationName = Core.Nothing,
        launchTemplate = Core.Nothing,
        weightedCapacity = Core.Nothing
      }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidInstanceId :: Lens.Lens' AutoScalingInstanceDetails Types.XmlStringMaxLen19
asidInstanceId = Lens.field @"instanceId"
{-# DEPRECATED asidInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the Auto Scaling group for the instance.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidAutoScalingGroupName :: Lens.Lens' AutoScalingInstanceDetails Types.XmlStringMaxLen255
asidAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED asidAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The Availability Zone for the instance.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidAvailabilityZone :: Lens.Lens' AutoScalingInstanceDetails Types.XmlStringMaxLen255
asidAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED asidAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The lifecycle state for the instance.
--
-- /Note:/ Consider using 'lifecycleState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidLifecycleState :: Lens.Lens' AutoScalingInstanceDetails Types.XmlStringMaxLen32
asidLifecycleState = Lens.field @"lifecycleState"
{-# DEPRECATED asidLifecycleState "Use generic-lens or generic-optics with 'lifecycleState' instead." #-}

-- | The last reported health status of this instance. "Healthy" means that the instance is healthy and should remain in service. "Unhealthy" means that the instance is unhealthy and Amazon EC2 Auto Scaling should terminate and replace it.
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidHealthStatus :: Lens.Lens' AutoScalingInstanceDetails Types.XmlStringMaxLen32
asidHealthStatus = Lens.field @"healthStatus"
{-# DEPRECATED asidHealthStatus "Use generic-lens or generic-optics with 'healthStatus' instead." #-}

-- | Indicates whether the instance is protected from termination by Amazon EC2 Auto Scaling when scaling in.
--
-- /Note:/ Consider using 'protectedFromScaleIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidProtectedFromScaleIn :: Lens.Lens' AutoScalingInstanceDetails Core.Bool
asidProtectedFromScaleIn = Lens.field @"protectedFromScaleIn"
{-# DEPRECATED asidProtectedFromScaleIn "Use generic-lens or generic-optics with 'protectedFromScaleIn' instead." #-}

-- | The instance type of the EC2 instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidInstanceType :: Lens.Lens' AutoScalingInstanceDetails (Core.Maybe Types.XmlStringMaxLen255)
asidInstanceType = Lens.field @"instanceType"
{-# DEPRECATED asidInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The launch configuration used to launch the instance. This value is not available if you attached the instance to the Auto Scaling group.
--
-- /Note:/ Consider using 'launchConfigurationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidLaunchConfigurationName :: Lens.Lens' AutoScalingInstanceDetails (Core.Maybe Types.XmlStringMaxLen255)
asidLaunchConfigurationName = Lens.field @"launchConfigurationName"
{-# DEPRECATED asidLaunchConfigurationName "Use generic-lens or generic-optics with 'launchConfigurationName' instead." #-}

-- | The launch template for the instance.
--
-- /Note:/ Consider using 'launchTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidLaunchTemplate :: Lens.Lens' AutoScalingInstanceDetails (Core.Maybe Types.LaunchTemplateSpecification)
asidLaunchTemplate = Lens.field @"launchTemplate"
{-# DEPRECATED asidLaunchTemplate "Use generic-lens or generic-optics with 'launchTemplate' instead." #-}

-- | The number of capacity units contributed by the instance based on its instance type.
--
-- Valid Range: Minimum value of 1. Maximum value of 999.
--
-- /Note:/ Consider using 'weightedCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asidWeightedCapacity :: Lens.Lens' AutoScalingInstanceDetails (Core.Maybe Types.XmlStringMaxLen32)
asidWeightedCapacity = Lens.field @"weightedCapacity"
{-# DEPRECATED asidWeightedCapacity "Use generic-lens or generic-optics with 'weightedCapacity' instead." #-}

instance Core.FromXML AutoScalingInstanceDetails where
  parseXML x =
    AutoScalingInstanceDetails'
      Core.<$> (x Core..@ "InstanceId")
      Core.<*> (x Core..@ "AutoScalingGroupName")
      Core.<*> (x Core..@ "AvailabilityZone")
      Core.<*> (x Core..@ "LifecycleState")
      Core.<*> (x Core..@ "HealthStatus")
      Core.<*> (x Core..@ "ProtectedFromScaleIn")
      Core.<*> (x Core..@? "InstanceType")
      Core.<*> (x Core..@? "LaunchConfigurationName")
      Core.<*> (x Core..@? "LaunchTemplate")
      Core.<*> (x Core..@? "WeightedCapacity")
