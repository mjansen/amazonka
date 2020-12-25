{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeAvailabilityZones
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Availability Zones, Local Zones, and Wavelength Zones that are available to you. If there is an event impacting a zone, you can use this request to view the state and any provided messages for that zone.
--
-- For more information about Availability Zones, Local Zones, and Wavelength Zones, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html Regions, Zones and Outposts> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribeAvailabilityZones
  ( -- * Creating a request
    DescribeAvailabilityZones (..),
    mkDescribeAvailabilityZones,

    -- ** Request lenses
    dazAllAvailabilityZones,
    dazDryRun,
    dazFilters,
    dazZoneIds,
    dazZoneNames,

    -- * Destructuring the response
    DescribeAvailabilityZonesResponse (..),
    mkDescribeAvailabilityZonesResponse,

    -- ** Response lenses
    dazrrsAvailabilityZones,
    dazrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeAvailabilityZones' smart constructor.
data DescribeAvailabilityZones = DescribeAvailabilityZones'
  { -- | Include all Availability Zones, Local Zones, and Wavelength Zones regardless of your opt-in status.
    --
    -- If you do not use this parameter, the results include only the zones for the Regions where you have chosen the option to opt in.
    allAvailabilityZones :: Core.Maybe Core.Bool,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | The filters.
    --
    --
    --     * @group-name@ - For Availability Zones, use the Region name. For Local Zones, use the name of the group associated with the Local Zone (for example, @us-west-2-lax-1@ ) For Wavelength Zones, use the name of the group associated with the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).
    --
    --
    --     * @message@ - The Zone message.
    --
    --
    --     * @opt-in-status@ - The opt-in status (@opted-in@ , and @not-opted-in@ | @opt-in-not-required@ ).
    --
    --
    --     * @parent-zoneID@ - The ID of the zone that handles some of the Local Zone and Wavelength Zone control plane operations, such as API calls.
    --
    --
    --     * @parent-zoneName@ - The ID of the zone that handles some of the Local Zone and Wavelength Zone control plane operations, such as API calls.
    --
    --
    --     * @region-name@ - The name of the Region for the Zone (for example, @us-east-1@ ).
    --
    --
    --     * @state@ - The state of the Availability Zone, the Local Zone, or the Wavelength Zone (@available@ | @information@ | @impaired@ | @unavailable@ ).
    --
    --
    --     * @zone-id@ - The ID of the Availability Zone (for example, @use1-az1@ ), the Local Zone (for example, @usw2-lax1-az1@ ), or the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).
    --
    --
    --     * @zone-type@ - The type of zone, for example, @local-zone@ .
    --
    --
    --     * @zone-name@ - The name of the Availability Zone (for example, @us-east-1a@ ), the Local Zone (for example, @us-west-2-lax-1a@ ), or the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).
    --
    --
    --     * @zone-type@ - The type of zone, for example, @local-zone@ .
    filters :: Core.Maybe [Types.Filter],
    -- | The IDs of the Availability Zones, Local Zones, and Wavelength Zones.
    zoneIds :: Core.Maybe [Types.String],
    -- | The names of the Availability Zones, Local Zones, and Wavelength Zones.
    zoneNames :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAvailabilityZones' value with any optional fields omitted.
mkDescribeAvailabilityZones ::
  DescribeAvailabilityZones
mkDescribeAvailabilityZones =
  DescribeAvailabilityZones'
    { allAvailabilityZones = Core.Nothing,
      dryRun = Core.Nothing,
      filters = Core.Nothing,
      zoneIds = Core.Nothing,
      zoneNames = Core.Nothing
    }

-- | Include all Availability Zones, Local Zones, and Wavelength Zones regardless of your opt-in status.
--
-- If you do not use this parameter, the results include only the zones for the Regions where you have chosen the option to opt in.
--
-- /Note:/ Consider using 'allAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazAllAvailabilityZones :: Lens.Lens' DescribeAvailabilityZones (Core.Maybe Core.Bool)
dazAllAvailabilityZones = Lens.field @"allAvailabilityZones"
{-# DEPRECATED dazAllAvailabilityZones "Use generic-lens or generic-optics with 'allAvailabilityZones' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazDryRun :: Lens.Lens' DescribeAvailabilityZones (Core.Maybe Core.Bool)
dazDryRun = Lens.field @"dryRun"
{-# DEPRECATED dazDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The filters.
--
--
--     * @group-name@ - For Availability Zones, use the Region name. For Local Zones, use the name of the group associated with the Local Zone (for example, @us-west-2-lax-1@ ) For Wavelength Zones, use the name of the group associated with the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).
--
--
--     * @message@ - The Zone message.
--
--
--     * @opt-in-status@ - The opt-in status (@opted-in@ , and @not-opted-in@ | @opt-in-not-required@ ).
--
--
--     * @parent-zoneID@ - The ID of the zone that handles some of the Local Zone and Wavelength Zone control plane operations, such as API calls.
--
--
--     * @parent-zoneName@ - The ID of the zone that handles some of the Local Zone and Wavelength Zone control plane operations, such as API calls.
--
--
--     * @region-name@ - The name of the Region for the Zone (for example, @us-east-1@ ).
--
--
--     * @state@ - The state of the Availability Zone, the Local Zone, or the Wavelength Zone (@available@ | @information@ | @impaired@ | @unavailable@ ).
--
--
--     * @zone-id@ - The ID of the Availability Zone (for example, @use1-az1@ ), the Local Zone (for example, @usw2-lax1-az1@ ), or the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).
--
--
--     * @zone-type@ - The type of zone, for example, @local-zone@ .
--
--
--     * @zone-name@ - The name of the Availability Zone (for example, @us-east-1a@ ), the Local Zone (for example, @us-west-2-lax-1a@ ), or the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).
--
--
--     * @zone-type@ - The type of zone, for example, @local-zone@ .
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazFilters :: Lens.Lens' DescribeAvailabilityZones (Core.Maybe [Types.Filter])
dazFilters = Lens.field @"filters"
{-# DEPRECATED dazFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The IDs of the Availability Zones, Local Zones, and Wavelength Zones.
--
-- /Note:/ Consider using 'zoneIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazZoneIds :: Lens.Lens' DescribeAvailabilityZones (Core.Maybe [Types.String])
dazZoneIds = Lens.field @"zoneIds"
{-# DEPRECATED dazZoneIds "Use generic-lens or generic-optics with 'zoneIds' instead." #-}

-- | The names of the Availability Zones, Local Zones, and Wavelength Zones.
--
-- /Note:/ Consider using 'zoneNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazZoneNames :: Lens.Lens' DescribeAvailabilityZones (Core.Maybe [Types.String])
dazZoneNames = Lens.field @"zoneNames"
{-# DEPRECATED dazZoneNames "Use generic-lens or generic-optics with 'zoneNames' instead." #-}

instance Core.AWSRequest DescribeAvailabilityZones where
  type
    Rs DescribeAvailabilityZones =
      DescribeAvailabilityZonesResponse
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
            ( Core.pure ("Action", "DescribeAvailabilityZones")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> ( Core.toQueryValue "AllAvailabilityZones"
                            Core.<$> allAvailabilityZones
                        )
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryList "ZoneId" Core.<$> zoneIds)
                Core.<> (Core.toQueryList "ZoneName" Core.<$> zoneNames)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeAvailabilityZonesResponse'
            Core.<$> ( x Core..@? "availabilityZoneInfo"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAvailabilityZonesResponse' smart constructor.
data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse'
  { -- | Information about the Availability Zones, Local Zones, and Wavelength Zones.
    availabilityZones :: Core.Maybe [Types.AvailabilityZone],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAvailabilityZonesResponse' value with any optional fields omitted.
mkDescribeAvailabilityZonesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAvailabilityZonesResponse
mkDescribeAvailabilityZonesResponse responseStatus =
  DescribeAvailabilityZonesResponse'
    { availabilityZones =
        Core.Nothing,
      responseStatus
    }

-- | Information about the Availability Zones, Local Zones, and Wavelength Zones.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazrrsAvailabilityZones :: Lens.Lens' DescribeAvailabilityZonesResponse (Core.Maybe [Types.AvailabilityZone])
dazrrsAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED dazrrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazrrsResponseStatus :: Lens.Lens' DescribeAvailabilityZonesResponse Core.Int
dazrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dazrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
