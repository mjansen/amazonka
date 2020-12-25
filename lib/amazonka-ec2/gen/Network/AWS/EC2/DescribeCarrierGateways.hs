{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeCarrierGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your carrier gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeCarrierGateways
  ( -- * Creating a request
    DescribeCarrierGateways (..),
    mkDescribeCarrierGateways,

    -- ** Request lenses
    dcgsCarrierGatewayIds,
    dcgsDryRun,
    dcgsFilters,
    dcgsMaxResults,
    dcgsNextToken,

    -- * Destructuring the response
    DescribeCarrierGatewaysResponse (..),
    mkDescribeCarrierGatewaysResponse,

    -- ** Response lenses
    dcgrfrsCarrierGateways,
    dcgrfrsNextToken,
    dcgrfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCarrierGateways' smart constructor.
data DescribeCarrierGateways = DescribeCarrierGateways'
  { -- | One or more carrier gateway IDs.
    carrierGatewayIds :: Core.Maybe [Types.CarrierGatewayId],
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more filters.
    --
    --
    --     * @carrier-gateway-id@ - The ID of the carrier gateway.
    --
    --
    --     * @state@ - The state of the carrier gateway (@pending@ | @failed@ | @available@ | @deleting@ | @deleted@ ).
    --
    --
    --     * @owner-id@ - The AWS account ID of the owner of the carrier gateway.
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    --
    --
    --     * @vpc-id@ - The ID of the VPC associated with the carrier gateway.
    filters :: Core.Maybe [Types.Filter],
    -- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next page of results.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCarrierGateways' value with any optional fields omitted.
mkDescribeCarrierGateways ::
  DescribeCarrierGateways
mkDescribeCarrierGateways =
  DescribeCarrierGateways'
    { carrierGatewayIds = Core.Nothing,
      dryRun = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | One or more carrier gateway IDs.
--
-- /Note:/ Consider using 'carrierGatewayIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgsCarrierGatewayIds :: Lens.Lens' DescribeCarrierGateways (Core.Maybe [Types.CarrierGatewayId])
dcgsCarrierGatewayIds = Lens.field @"carrierGatewayIds"
{-# DEPRECATED dcgsCarrierGatewayIds "Use generic-lens or generic-optics with 'carrierGatewayIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgsDryRun :: Lens.Lens' DescribeCarrierGateways (Core.Maybe Core.Bool)
dcgsDryRun = Lens.field @"dryRun"
{-# DEPRECATED dcgsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | One or more filters.
--
--
--     * @carrier-gateway-id@ - The ID of the carrier gateway.
--
--
--     * @state@ - The state of the carrier gateway (@pending@ | @failed@ | @available@ | @deleting@ | @deleted@ ).
--
--
--     * @owner-id@ - The AWS account ID of the owner of the carrier gateway.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @vpc-id@ - The ID of the VPC associated with the carrier gateway.
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgsFilters :: Lens.Lens' DescribeCarrierGateways (Core.Maybe [Types.Filter])
dcgsFilters = Lens.field @"filters"
{-# DEPRECATED dcgsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgsMaxResults :: Lens.Lens' DescribeCarrierGateways (Core.Maybe Core.Natural)
dcgsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dcgsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgsNextToken :: Lens.Lens' DescribeCarrierGateways (Core.Maybe Types.String)
dcgsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcgsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeCarrierGateways where
  type Rs DescribeCarrierGateways = DescribeCarrierGatewaysResponse
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
            ( Core.pure ("Action", "DescribeCarrierGateways")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "CarrierGatewayId" Core.<$> carrierGatewayIds)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeCarrierGatewaysResponse'
            Core.<$> (x Core..@? "carrierGatewaySet" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeCarrierGateways where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"carrierGateways" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeCarrierGatewaysResponse' smart constructor.
data DescribeCarrierGatewaysResponse = DescribeCarrierGatewaysResponse'
  { -- | Information about the carrier gateway.
    carrierGateways :: Core.Maybe [Types.CarrierGateway],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCarrierGatewaysResponse' value with any optional fields omitted.
mkDescribeCarrierGatewaysResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCarrierGatewaysResponse
mkDescribeCarrierGatewaysResponse responseStatus =
  DescribeCarrierGatewaysResponse'
    { carrierGateways = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the carrier gateway.
--
-- /Note:/ Consider using 'carrierGateways' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgrfrsCarrierGateways :: Lens.Lens' DescribeCarrierGatewaysResponse (Core.Maybe [Types.CarrierGateway])
dcgrfrsCarrierGateways = Lens.field @"carrierGateways"
{-# DEPRECATED dcgrfrsCarrierGateways "Use generic-lens or generic-optics with 'carrierGateways' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgrfrsNextToken :: Lens.Lens' DescribeCarrierGatewaysResponse (Core.Maybe Types.String)
dcgrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dcgrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgrfrsResponseStatus :: Lens.Lens' DescribeCarrierGatewaysResponse Core.Int
dcgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
