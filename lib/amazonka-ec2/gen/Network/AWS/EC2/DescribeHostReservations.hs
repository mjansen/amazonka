{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeHostReservations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes reservations that are associated with Dedicated Hosts in your account.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeHostReservations
  ( -- * Creating a request
    DescribeHostReservations (..),
    mkDescribeHostReservations,

    -- ** Request lenses
    dhrFilter,
    dhrHostReservationIdSet,
    dhrMaxResults,
    dhrNextToken,

    -- * Destructuring the response
    DescribeHostReservationsResponse (..),
    mkDescribeHostReservationsResponse,

    -- ** Response lenses
    dhrrrsHostReservationSet,
    dhrrrsNextToken,
    dhrrrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeHostReservations' smart constructor.
data DescribeHostReservations = DescribeHostReservations'
  { -- | The filters.
    --
    --
    --     * @instance-family@ - The instance family (for example, @m4@ ).
    --
    --
    --     * @payment-option@ - The payment option (@NoUpfront@ | @PartialUpfront@ | @AllUpfront@ ).
    --
    --
    --     * @state@ - The state of the reservation (@payment-pending@ | @payment-failed@ | @active@ | @retired@ ).
    --
    --
    --     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
    --
    --
    --     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
    filter :: Core.Maybe [Types.Filter],
    -- | The host reservation IDs.
    hostReservationIdSet :: Core.Maybe [Types.HostReservationId],
    -- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
    maxResults :: Core.Maybe Core.Int,
    -- | The token to use to retrieve the next page of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeHostReservations' value with any optional fields omitted.
mkDescribeHostReservations ::
  DescribeHostReservations
mkDescribeHostReservations =
  DescribeHostReservations'
    { filter = Core.Nothing,
      hostReservationIdSet = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The filters.
--
--
--     * @instance-family@ - The instance family (for example, @m4@ ).
--
--
--     * @payment-option@ - The payment option (@NoUpfront@ | @PartialUpfront@ | @AllUpfront@ ).
--
--
--     * @state@ - The state of the reservation (@payment-pending@ | @payment-failed@ | @active@ | @retired@ ).
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrFilter :: Lens.Lens' DescribeHostReservations (Core.Maybe [Types.Filter])
dhrFilter = Lens.field @"filter"
{-# DEPRECATED dhrFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The host reservation IDs.
--
-- /Note:/ Consider using 'hostReservationIdSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrHostReservationIdSet :: Lens.Lens' DescribeHostReservations (Core.Maybe [Types.HostReservationId])
dhrHostReservationIdSet = Lens.field @"hostReservationIdSet"
{-# DEPRECATED dhrHostReservationIdSet "Use generic-lens or generic-optics with 'hostReservationIdSet' instead." #-}

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @nextToken@ value. This value can be between 5 and 500. If @maxResults@ is given a larger value than 500, you receive an error.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrMaxResults :: Lens.Lens' DescribeHostReservations (Core.Maybe Core.Int)
dhrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dhrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token to use to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrNextToken :: Lens.Lens' DescribeHostReservations (Core.Maybe Types.NextToken)
dhrNextToken = Lens.field @"nextToken"
{-# DEPRECATED dhrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeHostReservations where
  type Rs DescribeHostReservations = DescribeHostReservationsResponse
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
            ( Core.pure ("Action", "DescribeHostReservations")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryList "Filter" Core.<$> filter)
                Core.<> ( Core.toQueryList "HostReservationIdSet"
                            Core.<$> hostReservationIdSet
                        )
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeHostReservationsResponse'
            Core.<$> ( x Core..@? "hostReservationSet"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeHostReservations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"hostReservationSet" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeHostReservationsResponse' smart constructor.
data DescribeHostReservationsResponse = DescribeHostReservationsResponse'
  { -- | Details about the reservation's configuration.
    hostReservationSet :: Core.Maybe [Types.HostReservation],
    -- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeHostReservationsResponse' value with any optional fields omitted.
mkDescribeHostReservationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeHostReservationsResponse
mkDescribeHostReservationsResponse responseStatus =
  DescribeHostReservationsResponse'
    { hostReservationSet =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Details about the reservation's configuration.
--
-- /Note:/ Consider using 'hostReservationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrrsHostReservationSet :: Lens.Lens' DescribeHostReservationsResponse (Core.Maybe [Types.HostReservation])
dhrrrsHostReservationSet = Lens.field @"hostReservationSet"
{-# DEPRECATED dhrrrsHostReservationSet "Use generic-lens or generic-optics with 'hostReservationSet' instead." #-}

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrrsNextToken :: Lens.Lens' DescribeHostReservationsResponse (Core.Maybe Types.String)
dhrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dhrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrrrsResponseStatus :: Lens.Lens' DescribeHostReservationsResponse Core.Int
dhrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dhrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
