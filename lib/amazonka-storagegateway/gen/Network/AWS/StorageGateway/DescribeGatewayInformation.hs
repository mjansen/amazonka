{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeGatewayInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about a gateway such as its name, network interfaces, configured time zone, and the state (whether the gateway is running or not). To specify which gateway to describe, use the Amazon Resource Name (ARN) of the gateway in your request.
module Network.AWS.StorageGateway.DescribeGatewayInformation
  ( -- * Creating a request
    DescribeGatewayInformation (..),
    mkDescribeGatewayInformation,

    -- ** Request lenses
    dgiGatewayARN,

    -- * Destructuring the response
    DescribeGatewayInformationResponse (..),
    mkDescribeGatewayInformationResponse,

    -- ** Response lenses
    dgirrsCloudWatchLogGroupARN,
    dgirrsDeprecationDate,
    dgirrsEc2InstanceId,
    dgirrsEc2InstanceRegion,
    dgirrsEndpointType,
    dgirrsGatewayARN,
    dgirrsGatewayId,
    dgirrsGatewayName,
    dgirrsGatewayNetworkInterfaces,
    dgirrsGatewayState,
    dgirrsGatewayTimezone,
    dgirrsGatewayType,
    dgirrsHostEnvironment,
    dgirrsLastSoftwareUpdate,
    dgirrsNextUpdateAvailabilityDate,
    dgirrsSoftwareUpdatesEndDate,
    dgirrsTags,
    dgirrsVPCEndpoint,
    dgirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing the ID of the gateway.
--
-- /See:/ 'mkDescribeGatewayInformation' smart constructor.
newtype DescribeGatewayInformation = DescribeGatewayInformation'
  { gatewayARN :: Types.GatewayARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGatewayInformation' value with any optional fields omitted.
mkDescribeGatewayInformation ::
  -- | 'gatewayARN'
  Types.GatewayARN ->
  DescribeGatewayInformation
mkDescribeGatewayInformation gatewayARN =
  DescribeGatewayInformation' {gatewayARN}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgiGatewayARN :: Lens.Lens' DescribeGatewayInformation Types.GatewayARN
dgiGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED dgiGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Core.FromJSON DescribeGatewayInformation where
  toJSON DescribeGatewayInformation {..} =
    Core.object
      (Core.catMaybes [Core.Just ("GatewayARN" Core..= gatewayARN)])

instance Core.AWSRequest DescribeGatewayInformation where
  type
    Rs DescribeGatewayInformation =
      DescribeGatewayInformationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StorageGateway_20130630.DescribeGatewayInformation"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGatewayInformationResponse'
            Core.<$> (x Core..:? "CloudWatchLogGroupARN")
            Core.<*> (x Core..:? "DeprecationDate")
            Core.<*> (x Core..:? "Ec2InstanceId")
            Core.<*> (x Core..:? "Ec2InstanceRegion")
            Core.<*> (x Core..:? "EndpointType")
            Core.<*> (x Core..:? "GatewayARN")
            Core.<*> (x Core..:? "GatewayId")
            Core.<*> (x Core..:? "GatewayName")
            Core.<*> (x Core..:? "GatewayNetworkInterfaces")
            Core.<*> (x Core..:? "GatewayState")
            Core.<*> (x Core..:? "GatewayTimezone")
            Core.<*> (x Core..:? "GatewayType")
            Core.<*> (x Core..:? "HostEnvironment")
            Core.<*> (x Core..:? "LastSoftwareUpdate")
            Core.<*> (x Core..:? "NextUpdateAvailabilityDate")
            Core.<*> (x Core..:? "SoftwareUpdatesEndDate")
            Core.<*> (x Core..:? "Tags")
            Core.<*> (x Core..:? "VPCEndpoint")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkDescribeGatewayInformationResponse' smart constructor.
data DescribeGatewayInformationResponse = DescribeGatewayInformationResponse'
  { -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that is used to monitor events in the gateway.
    cloudWatchLogGroupARN :: Core.Maybe Types.CloudWatchLogGroupARN,
    -- | Date after which this gateway will not receive software updates for new features and bug fixes.
    deprecationDate :: Core.Maybe Types.DeprecationDate,
    -- | The ID of the Amazon EC2 instance that was used to launch the gateway.
    ec2InstanceId :: Core.Maybe Types.Ec2InstanceId,
    -- | The AWS Region where the Amazon EC2 instance is located.
    ec2InstanceRegion :: Core.Maybe Types.Ec2InstanceRegion,
    -- | The type of endpoint for your gateway.
    --
    -- Valid Values: @STANDARD@ | @FIPS@
    endpointType :: Core.Maybe Types.EndpointType,
    gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
    gatewayId :: Core.Maybe Types.GatewayId,
    -- | The name you configured for your gateway.
    gatewayName :: Core.Maybe Types.String,
    -- | A 'NetworkInterface' array that contains descriptions of the gateway network interfaces.
    gatewayNetworkInterfaces :: Core.Maybe [Types.NetworkInterface],
    -- | A value that indicates the operating state of the gateway.
    gatewayState :: Core.Maybe Types.GatewayState,
    -- | A value that indicates the time zone configured for the gateway.
    gatewayTimezone :: Core.Maybe Types.GatewayTimezone,
    -- | The type of the gateway.
    gatewayType :: Core.Maybe Types.GatewayType,
    -- | The type of hypervisor environment used by the host.
    hostEnvironment :: Core.Maybe Types.HostEnvironment,
    -- | The date on which the last software update was applied to the gateway. If the gateway has never been updated, this field does not return a value in the response.
    lastSoftwareUpdate :: Core.Maybe Types.LastSoftwareUpdate,
    -- | The date on which an update to the gateway is available. This date is in the time zone of the gateway. If the gateway is not available for an update this field is not returned in the response.
    nextUpdateAvailabilityDate :: Core.Maybe Types.NextUpdateAvailabilityDate,
    -- | Date after which this gateway will not receive software updates for new features.
    softwareUpdatesEndDate :: Core.Maybe Types.SoftwareUpdatesEndDate,
    -- | A list of up to 50 tags assigned to the gateway, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
    tags :: Core.Maybe [Types.Tag],
    -- | The configuration settings for the virtual private cloud (VPC) endpoint for your gateway.
    vPCEndpoint :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGatewayInformationResponse' value with any optional fields omitted.
mkDescribeGatewayInformationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeGatewayInformationResponse
mkDescribeGatewayInformationResponse responseStatus =
  DescribeGatewayInformationResponse'
    { cloudWatchLogGroupARN =
        Core.Nothing,
      deprecationDate = Core.Nothing,
      ec2InstanceId = Core.Nothing,
      ec2InstanceRegion = Core.Nothing,
      endpointType = Core.Nothing,
      gatewayARN = Core.Nothing,
      gatewayId = Core.Nothing,
      gatewayName = Core.Nothing,
      gatewayNetworkInterfaces = Core.Nothing,
      gatewayState = Core.Nothing,
      gatewayTimezone = Core.Nothing,
      gatewayType = Core.Nothing,
      hostEnvironment = Core.Nothing,
      lastSoftwareUpdate = Core.Nothing,
      nextUpdateAvailabilityDate = Core.Nothing,
      softwareUpdatesEndDate = Core.Nothing,
      tags = Core.Nothing,
      vPCEndpoint = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch log group that is used to monitor events in the gateway.
--
-- /Note:/ Consider using 'cloudWatchLogGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsCloudWatchLogGroupARN :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.CloudWatchLogGroupARN)
dgirrsCloudWatchLogGroupARN = Lens.field @"cloudWatchLogGroupARN"
{-# DEPRECATED dgirrsCloudWatchLogGroupARN "Use generic-lens or generic-optics with 'cloudWatchLogGroupARN' instead." #-}

-- | Date after which this gateway will not receive software updates for new features and bug fixes.
--
-- /Note:/ Consider using 'deprecationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsDeprecationDate :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.DeprecationDate)
dgirrsDeprecationDate = Lens.field @"deprecationDate"
{-# DEPRECATED dgirrsDeprecationDate "Use generic-lens or generic-optics with 'deprecationDate' instead." #-}

-- | The ID of the Amazon EC2 instance that was used to launch the gateway.
--
-- /Note:/ Consider using 'ec2InstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsEc2InstanceId :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.Ec2InstanceId)
dgirrsEc2InstanceId = Lens.field @"ec2InstanceId"
{-# DEPRECATED dgirrsEc2InstanceId "Use generic-lens or generic-optics with 'ec2InstanceId' instead." #-}

-- | The AWS Region where the Amazon EC2 instance is located.
--
-- /Note:/ Consider using 'ec2InstanceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsEc2InstanceRegion :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.Ec2InstanceRegion)
dgirrsEc2InstanceRegion = Lens.field @"ec2InstanceRegion"
{-# DEPRECATED dgirrsEc2InstanceRegion "Use generic-lens or generic-optics with 'ec2InstanceRegion' instead." #-}

-- | The type of endpoint for your gateway.
--
-- Valid Values: @STANDARD@ | @FIPS@
--
-- /Note:/ Consider using 'endpointType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsEndpointType :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.EndpointType)
dgirrsEndpointType = Lens.field @"endpointType"
{-# DEPRECATED dgirrsEndpointType "Use generic-lens or generic-optics with 'endpointType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsGatewayARN :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.GatewayARN)
dgirrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED dgirrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The unique identifier assigned to your gateway during activation. This ID becomes part of the gateway Amazon Resource Name (ARN), which you use as input for other operations.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsGatewayId :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.GatewayId)
dgirrsGatewayId = Lens.field @"gatewayId"
{-# DEPRECATED dgirrsGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

-- | The name you configured for your gateway.
--
-- /Note:/ Consider using 'gatewayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsGatewayName :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.String)
dgirrsGatewayName = Lens.field @"gatewayName"
{-# DEPRECATED dgirrsGatewayName "Use generic-lens or generic-optics with 'gatewayName' instead." #-}

-- | A 'NetworkInterface' array that contains descriptions of the gateway network interfaces.
--
-- /Note:/ Consider using 'gatewayNetworkInterfaces' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsGatewayNetworkInterfaces :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe [Types.NetworkInterface])
dgirrsGatewayNetworkInterfaces = Lens.field @"gatewayNetworkInterfaces"
{-# DEPRECATED dgirrsGatewayNetworkInterfaces "Use generic-lens or generic-optics with 'gatewayNetworkInterfaces' instead." #-}

-- | A value that indicates the operating state of the gateway.
--
-- /Note:/ Consider using 'gatewayState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsGatewayState :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.GatewayState)
dgirrsGatewayState = Lens.field @"gatewayState"
{-# DEPRECATED dgirrsGatewayState "Use generic-lens or generic-optics with 'gatewayState' instead." #-}

-- | A value that indicates the time zone configured for the gateway.
--
-- /Note:/ Consider using 'gatewayTimezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsGatewayTimezone :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.GatewayTimezone)
dgirrsGatewayTimezone = Lens.field @"gatewayTimezone"
{-# DEPRECATED dgirrsGatewayTimezone "Use generic-lens or generic-optics with 'gatewayTimezone' instead." #-}

-- | The type of the gateway.
--
-- /Note:/ Consider using 'gatewayType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsGatewayType :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.GatewayType)
dgirrsGatewayType = Lens.field @"gatewayType"
{-# DEPRECATED dgirrsGatewayType "Use generic-lens or generic-optics with 'gatewayType' instead." #-}

-- | The type of hypervisor environment used by the host.
--
-- /Note:/ Consider using 'hostEnvironment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsHostEnvironment :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.HostEnvironment)
dgirrsHostEnvironment = Lens.field @"hostEnvironment"
{-# DEPRECATED dgirrsHostEnvironment "Use generic-lens or generic-optics with 'hostEnvironment' instead." #-}

-- | The date on which the last software update was applied to the gateway. If the gateway has never been updated, this field does not return a value in the response.
--
-- /Note:/ Consider using 'lastSoftwareUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsLastSoftwareUpdate :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.LastSoftwareUpdate)
dgirrsLastSoftwareUpdate = Lens.field @"lastSoftwareUpdate"
{-# DEPRECATED dgirrsLastSoftwareUpdate "Use generic-lens or generic-optics with 'lastSoftwareUpdate' instead." #-}

-- | The date on which an update to the gateway is available. This date is in the time zone of the gateway. If the gateway is not available for an update this field is not returned in the response.
--
-- /Note:/ Consider using 'nextUpdateAvailabilityDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsNextUpdateAvailabilityDate :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.NextUpdateAvailabilityDate)
dgirrsNextUpdateAvailabilityDate = Lens.field @"nextUpdateAvailabilityDate"
{-# DEPRECATED dgirrsNextUpdateAvailabilityDate "Use generic-lens or generic-optics with 'nextUpdateAvailabilityDate' instead." #-}

-- | Date after which this gateway will not receive software updates for new features.
--
-- /Note:/ Consider using 'softwareUpdatesEndDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsSoftwareUpdatesEndDate :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.SoftwareUpdatesEndDate)
dgirrsSoftwareUpdatesEndDate = Lens.field @"softwareUpdatesEndDate"
{-# DEPRECATED dgirrsSoftwareUpdatesEndDate "Use generic-lens or generic-optics with 'softwareUpdatesEndDate' instead." #-}

-- | A list of up to 50 tags assigned to the gateway, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsTags :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe [Types.Tag])
dgirrsTags = Lens.field @"tags"
{-# DEPRECATED dgirrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The configuration settings for the virtual private cloud (VPC) endpoint for your gateway.
--
-- /Note:/ Consider using 'vPCEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsVPCEndpoint :: Lens.Lens' DescribeGatewayInformationResponse (Core.Maybe Types.String)
dgirrsVPCEndpoint = Lens.field @"vPCEndpoint"
{-# DEPRECATED dgirrsVPCEndpoint "Use generic-lens or generic-optics with 'vPCEndpoint' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgirrsResponseStatus :: Lens.Lens' DescribeGatewayInformationResponse Core.Int
dgirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dgirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
