{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RequestSpotInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Spot Instance request.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/spot-requests.html Spot Instance requests> in the /Amazon EC2 User Guide for Linux Instances/ .
module Network.AWS.EC2.RequestSpotInstances
  ( -- * Creating a request
    RequestSpotInstances (..),
    mkRequestSpotInstances,

    -- ** Request lenses
    rsisAvailabilityZoneGroup,
    rsisBlockDurationMinutes,
    rsisClientToken,
    rsisDryRun,
    rsisInstanceCount,
    rsisInstanceInterruptionBehavior,
    rsisLaunchGroup,
    rsisLaunchSpecification,
    rsisSpotPrice,
    rsisTagSpecifications,
    rsisType,
    rsisValidFrom,
    rsisValidUntil,

    -- * Destructuring the response
    RequestSpotInstancesResponse (..),
    mkRequestSpotInstancesResponse,

    -- ** Response lenses
    rrsSpotInstanceRequests,
    rrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for RequestSpotInstances.
--
-- /See:/ 'mkRequestSpotInstances' smart constructor.
data RequestSpotInstances = RequestSpotInstances'
  { -- | The user-specified name for a logical grouping of requests.
    --
    -- When you specify an Availability Zone group in a Spot Instance request, all Spot Instances in the request are launched in the same Availability Zone. Instance proximity is maintained with this parameter, but the choice of Availability Zone is not. The group applies only to requests for Spot Instances of the same instance type. Any additional Spot Instance requests that are specified with the same Availability Zone group name are launched in that same Availability Zone, as long as at least one instance from the group is still active.
    -- If there is no active instance running in the Availability Zone group that you specify for a new Spot Instance request (all instances are terminated, the request is expired, or the maximum price you specified falls below current Spot price), then Amazon EC2 launches the instance in any Availability Zone where the constraint can be met. Consequently, the subsequent set of Spot Instances could be placed in a different zone from the original request, even if you specified the same Availability Zone group.
    -- Default: Instances are launched in any available Availability Zone.
    availabilityZoneGroup :: Core.Maybe Types.AvailabilityZoneGroup,
    -- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
    --
    -- The duration period starts as soon as your Spot Instance receives its instance ID. At the end of the duration period, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates.
    -- You can't specify an Availability Zone group or a launch group if you specify a duration.
    -- New accounts or accounts with no previous billing history with AWS are not eligible for Spot Instances with a defined duration (also known as Spot blocks).
    blockDurationMinutes :: Core.Maybe Core.Int,
    -- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon EC2 User Guide for Linux Instances/ .
    clientToken :: Core.Maybe Types.ClientToken,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of Spot Instances to launch.
    --
    -- Default: 1
    instanceCount :: Core.Maybe Core.Int,
    -- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
    instanceInterruptionBehavior :: Core.Maybe Types.InstanceInterruptionBehavior,
    -- | The instance launch group. Launch groups are Spot Instances that launch together and terminate together.
    --
    -- Default: Instances are launched and terminated individually
    launchGroup :: Core.Maybe Types.LaunchGroup,
    -- | The launch specification.
    launchSpecification :: Core.Maybe Types.RequestSpotLaunchSpecification,
    -- | The maximum price per hour that you are willing to pay for a Spot Instance. The default is the On-Demand price.
    spotPrice :: Core.Maybe Types.String,
    -- | The key-value pair for tagging the Spot Instance request on creation. The value for @ResourceType@ must be @spot-instances-request@ , otherwise the Spot Instance request fails. To tag the Spot Instance request after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
    tagSpecifications :: Core.Maybe [Types.TagSpecification],
    -- | The Spot Instance request type.
    --
    -- Default: @one-time@
    type' :: Core.Maybe Types.SpotInstanceType,
    -- | The start date of the request. If this is a one-time request, the request becomes active at this date and time and remains active until all instances launch, the request expires, or the request is canceled. If the request is persistent, the request becomes active at this date and time and remains active until it expires or is canceled.
    --
    -- The specified start date and time cannot be equal to the current date and time. You must specify a start date and time that occurs after the current date and time.
    validFrom :: Core.Maybe Core.UTCTime,
    -- | The end date of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
    --
    --
    --     * For a persistent request, the request remains active until the @ValidUntil@ date and time is reached. Otherwise, the request remains active until you cancel it.
    --
    --
    --     * For a one-time request, the request remains active until all instances launch, the request is canceled, or the @ValidUntil@ date and time is reached. By default, the request is valid for 7 days from the date the request was created.
    validUntil :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RequestSpotInstances' value with any optional fields omitted.
mkRequestSpotInstances ::
  RequestSpotInstances
mkRequestSpotInstances =
  RequestSpotInstances'
    { availabilityZoneGroup = Core.Nothing,
      blockDurationMinutes = Core.Nothing,
      clientToken = Core.Nothing,
      dryRun = Core.Nothing,
      instanceCount = Core.Nothing,
      instanceInterruptionBehavior = Core.Nothing,
      launchGroup = Core.Nothing,
      launchSpecification = Core.Nothing,
      spotPrice = Core.Nothing,
      tagSpecifications = Core.Nothing,
      type' = Core.Nothing,
      validFrom = Core.Nothing,
      validUntil = Core.Nothing
    }

-- | The user-specified name for a logical grouping of requests.
--
-- When you specify an Availability Zone group in a Spot Instance request, all Spot Instances in the request are launched in the same Availability Zone. Instance proximity is maintained with this parameter, but the choice of Availability Zone is not. The group applies only to requests for Spot Instances of the same instance type. Any additional Spot Instance requests that are specified with the same Availability Zone group name are launched in that same Availability Zone, as long as at least one instance from the group is still active.
-- If there is no active instance running in the Availability Zone group that you specify for a new Spot Instance request (all instances are terminated, the request is expired, or the maximum price you specified falls below current Spot price), then Amazon EC2 launches the instance in any Availability Zone where the constraint can be met. Consequently, the subsequent set of Spot Instances could be placed in a different zone from the original request, even if you specified the same Availability Zone group.
-- Default: Instances are launched in any available Availability Zone.
--
-- /Note:/ Consider using 'availabilityZoneGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsisAvailabilityZoneGroup :: Lens.Lens' RequestSpotInstances (Core.Maybe Types.AvailabilityZoneGroup)
rsisAvailabilityZoneGroup = Lens.field @"availabilityZoneGroup"
{-# DEPRECATED rsisAvailabilityZoneGroup "Use generic-lens or generic-optics with 'availabilityZoneGroup' instead." #-}

-- | The required duration for the Spot Instances (also known as Spot blocks), in minutes. This value must be a multiple of 60 (60, 120, 180, 240, 300, or 360).
--
-- The duration period starts as soon as your Spot Instance receives its instance ID. At the end of the duration period, Amazon EC2 marks the Spot Instance for termination and provides a Spot Instance termination notice, which gives the instance a two-minute warning before it terminates.
-- You can't specify an Availability Zone group or a launch group if you specify a duration.
-- New accounts or accounts with no previous billing history with AWS are not eligible for Spot Instances with a defined duration (also known as Spot blocks).
--
-- /Note:/ Consider using 'blockDurationMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsisBlockDurationMinutes :: Lens.Lens' RequestSpotInstances (Core.Maybe Core.Int)
rsisBlockDurationMinutes = Lens.field @"blockDurationMinutes"
{-# DEPRECATED rsisBlockDurationMinutes "Use generic-lens or generic-optics with 'blockDurationMinutes' instead." #-}

-- | Unique, case-sensitive identifier that you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsisClientToken :: Lens.Lens' RequestSpotInstances (Core.Maybe Types.ClientToken)
rsisClientToken = Lens.field @"clientToken"
{-# DEPRECATED rsisClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsisDryRun :: Lens.Lens' RequestSpotInstances (Core.Maybe Core.Bool)
rsisDryRun = Lens.field @"dryRun"
{-# DEPRECATED rsisDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The maximum number of Spot Instances to launch.
--
-- Default: 1
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsisInstanceCount :: Lens.Lens' RequestSpotInstances (Core.Maybe Core.Int)
rsisInstanceCount = Lens.field @"instanceCount"
{-# DEPRECATED rsisInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- /Note:/ Consider using 'instanceInterruptionBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsisInstanceInterruptionBehavior :: Lens.Lens' RequestSpotInstances (Core.Maybe Types.InstanceInterruptionBehavior)
rsisInstanceInterruptionBehavior = Lens.field @"instanceInterruptionBehavior"
{-# DEPRECATED rsisInstanceInterruptionBehavior "Use generic-lens or generic-optics with 'instanceInterruptionBehavior' instead." #-}

-- | The instance launch group. Launch groups are Spot Instances that launch together and terminate together.
--
-- Default: Instances are launched and terminated individually
--
-- /Note:/ Consider using 'launchGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsisLaunchGroup :: Lens.Lens' RequestSpotInstances (Core.Maybe Types.LaunchGroup)
rsisLaunchGroup = Lens.field @"launchGroup"
{-# DEPRECATED rsisLaunchGroup "Use generic-lens or generic-optics with 'launchGroup' instead." #-}

-- | The launch specification.
--
-- /Note:/ Consider using 'launchSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsisLaunchSpecification :: Lens.Lens' RequestSpotInstances (Core.Maybe Types.RequestSpotLaunchSpecification)
rsisLaunchSpecification = Lens.field @"launchSpecification"
{-# DEPRECATED rsisLaunchSpecification "Use generic-lens or generic-optics with 'launchSpecification' instead." #-}

-- | The maximum price per hour that you are willing to pay for a Spot Instance. The default is the On-Demand price.
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsisSpotPrice :: Lens.Lens' RequestSpotInstances (Core.Maybe Types.String)
rsisSpotPrice = Lens.field @"spotPrice"
{-# DEPRECATED rsisSpotPrice "Use generic-lens or generic-optics with 'spotPrice' instead." #-}

-- | The key-value pair for tagging the Spot Instance request on creation. The value for @ResourceType@ must be @spot-instances-request@ , otherwise the Spot Instance request fails. To tag the Spot Instance request after it has been created, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags> .
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsisTagSpecifications :: Lens.Lens' RequestSpotInstances (Core.Maybe [Types.TagSpecification])
rsisTagSpecifications = Lens.field @"tagSpecifications"
{-# DEPRECATED rsisTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The Spot Instance request type.
--
-- Default: @one-time@
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsisType :: Lens.Lens' RequestSpotInstances (Core.Maybe Types.SpotInstanceType)
rsisType = Lens.field @"type'"
{-# DEPRECATED rsisType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The start date of the request. If this is a one-time request, the request becomes active at this date and time and remains active until all instances launch, the request expires, or the request is canceled. If the request is persistent, the request becomes active at this date and time and remains active until it expires or is canceled.
--
-- The specified start date and time cannot be equal to the current date and time. You must specify a start date and time that occurs after the current date and time.
--
-- /Note:/ Consider using 'validFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsisValidFrom :: Lens.Lens' RequestSpotInstances (Core.Maybe Core.UTCTime)
rsisValidFrom = Lens.field @"validFrom"
{-# DEPRECATED rsisValidFrom "Use generic-lens or generic-optics with 'validFrom' instead." #-}

-- | The end date of the request, in UTC format (/YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
--
--     * For a persistent request, the request remains active until the @ValidUntil@ date and time is reached. Otherwise, the request remains active until you cancel it.
--
--
--     * For a one-time request, the request remains active until all instances launch, the request is canceled, or the @ValidUntil@ date and time is reached. By default, the request is valid for 7 days from the date the request was created.
--
--
--
-- /Note:/ Consider using 'validUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsisValidUntil :: Lens.Lens' RequestSpotInstances (Core.Maybe Core.UTCTime)
rsisValidUntil = Lens.field @"validUntil"
{-# DEPRECATED rsisValidUntil "Use generic-lens or generic-optics with 'validUntil' instead." #-}

instance Core.AWSRequest RequestSpotInstances where
  type Rs RequestSpotInstances = RequestSpotInstancesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "RequestSpotInstances")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue "AvailabilityZoneGroup"
                            Core.<$> availabilityZoneGroup
                        )
                Core.<> ( Core.toQueryValue "BlockDurationMinutes"
                            Core.<$> blockDurationMinutes
                        )
                Core.<> (Core.toQueryValue "ClientToken" Core.<$> clientToken)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "InstanceCount" Core.<$> instanceCount)
                Core.<> ( Core.toQueryValue "InstanceInterruptionBehavior"
                            Core.<$> instanceInterruptionBehavior
                        )
                Core.<> (Core.toQueryValue "LaunchGroup" Core.<$> launchGroup)
                Core.<> ( Core.toQueryValue "LaunchSpecification"
                            Core.<$> launchSpecification
                        )
                Core.<> (Core.toQueryValue "SpotPrice" Core.<$> spotPrice)
                Core.<> (Core.toQueryList "TagSpecification" Core.<$> tagSpecifications)
                Core.<> (Core.toQueryValue "Type" Core.<$> type')
                Core.<> (Core.toQueryValue "ValidFrom" Core.<$> validFrom)
                Core.<> (Core.toQueryValue "ValidUntil" Core.<$> validUntil)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          RequestSpotInstancesResponse'
            Core.<$> ( x Core..@? "spotInstanceRequestSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of RequestSpotInstances.
--
-- /See:/ 'mkRequestSpotInstancesResponse' smart constructor.
data RequestSpotInstancesResponse = RequestSpotInstancesResponse'
  { -- | One or more Spot Instance requests.
    spotInstanceRequests :: Core.Maybe [Types.SpotInstanceRequest],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RequestSpotInstancesResponse' value with any optional fields omitted.
mkRequestSpotInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RequestSpotInstancesResponse
mkRequestSpotInstancesResponse responseStatus =
  RequestSpotInstancesResponse'
    { spotInstanceRequests =
        Core.Nothing,
      responseStatus
    }

-- | One or more Spot Instance requests.
--
-- /Note:/ Consider using 'spotInstanceRequests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsSpotInstanceRequests :: Lens.Lens' RequestSpotInstancesResponse (Core.Maybe [Types.SpotInstanceRequest])
rrsSpotInstanceRequests = Lens.field @"spotInstanceRequests"
{-# DEPRECATED rrsSpotInstanceRequests "Use generic-lens or generic-optics with 'spotInstanceRequests' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RequestSpotInstancesResponse Core.Int
rrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
