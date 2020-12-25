{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.ViewBilling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all the domain-related billing records for the current AWS account for a specified period
--
-- This operation returns paginated results.
module Network.AWS.Route53Domains.ViewBilling
  ( -- * Creating a request
    ViewBilling (..),
    mkViewBilling,

    -- ** Request lenses
    vbEnd,
    vbMarker,
    vbMaxItems,
    vbStart,

    -- * Destructuring the response
    ViewBillingResponse (..),
    mkViewBillingResponse,

    -- ** Response lenses
    vbrrsBillingRecords,
    vbrrsNextPageMarker,
    vbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53Domains.Types as Types

-- | The ViewBilling request includes the following elements.
--
-- /See:/ 'mkViewBilling' smart constructor.
data ViewBilling = ViewBilling'
  { -- | The end date and time for the time period for which you want a list of billing records. Specify the date and time in Unix time format and Coordinated Universal time (UTC).
    end :: Core.Maybe Core.NominalDiffTime,
    -- | For an initial request for a list of billing records, omit this element. If the number of billing records that are associated with the current AWS account during the specified period is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional billing records. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
    --
    -- Constraints: The marker must match the value of @NextPageMarker@ that was returned in the previous response.
    marker :: Core.Maybe Types.Marker,
    -- | The number of billing records to be returned.
    --
    -- Default: 20
    maxItems :: Core.Maybe Core.Int,
    -- | The beginning date and time for the time period for which you want a list of billing records. Specify the date and time in Unix time format and Coordinated Universal time (UTC).
    start :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ViewBilling' value with any optional fields omitted.
mkViewBilling ::
  ViewBilling
mkViewBilling =
  ViewBilling'
    { end = Core.Nothing,
      marker = Core.Nothing,
      maxItems = Core.Nothing,
      start = Core.Nothing
    }

-- | The end date and time for the time period for which you want a list of billing records. Specify the date and time in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'end' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vbEnd :: Lens.Lens' ViewBilling (Core.Maybe Core.NominalDiffTime)
vbEnd = Lens.field @"end"
{-# DEPRECATED vbEnd "Use generic-lens or generic-optics with 'end' instead." #-}

-- | For an initial request for a list of billing records, omit this element. If the number of billing records that are associated with the current AWS account during the specified period is greater than the value that you specified for @MaxItems@ , you can use @Marker@ to return additional billing records. Get the value of @NextPageMarker@ from the previous response, and submit another request that includes the value of @NextPageMarker@ in the @Marker@ element.
--
-- Constraints: The marker must match the value of @NextPageMarker@ that was returned in the previous response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vbMarker :: Lens.Lens' ViewBilling (Core.Maybe Types.Marker)
vbMarker = Lens.field @"marker"
{-# DEPRECATED vbMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The number of billing records to be returned.
--
-- Default: 20
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vbMaxItems :: Lens.Lens' ViewBilling (Core.Maybe Core.Int)
vbMaxItems = Lens.field @"maxItems"
{-# DEPRECATED vbMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The beginning date and time for the time period for which you want a list of billing records. Specify the date and time in Unix time format and Coordinated Universal time (UTC).
--
-- /Note:/ Consider using 'start' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vbStart :: Lens.Lens' ViewBilling (Core.Maybe Core.NominalDiffTime)
vbStart = Lens.field @"start"
{-# DEPRECATED vbStart "Use generic-lens or generic-optics with 'start' instead." #-}

instance Core.FromJSON ViewBilling where
  toJSON ViewBilling {..} =
    Core.object
      ( Core.catMaybes
          [ ("End" Core..=) Core.<$> end,
            ("Marker" Core..=) Core.<$> marker,
            ("MaxItems" Core..=) Core.<$> maxItems,
            ("Start" Core..=) Core.<$> start
          ]
      )

instance Core.AWSRequest ViewBilling where
  type Rs ViewBilling = ViewBillingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Route53Domains_v20140515.ViewBilling")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ViewBillingResponse'
            Core.<$> (x Core..:? "BillingRecords")
            Core.<*> (x Core..:? "NextPageMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ViewBilling where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextPageMarker") =
      Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"billingRecords" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker"
            Lens..~ rs Lens.^. Lens.field @"nextPageMarker"
        )

-- | The ViewBilling response includes the following elements.
--
-- /See:/ 'mkViewBillingResponse' smart constructor.
data ViewBillingResponse = ViewBillingResponse'
  { -- | A summary of billing records.
    billingRecords :: Core.Maybe [Types.BillingRecord],
    -- | If there are more billing records than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
    nextPageMarker :: Core.Maybe Types.NextPageMarker,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ViewBillingResponse' value with any optional fields omitted.
mkViewBillingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ViewBillingResponse
mkViewBillingResponse responseStatus =
  ViewBillingResponse'
    { billingRecords = Core.Nothing,
      nextPageMarker = Core.Nothing,
      responseStatus
    }

-- | A summary of billing records.
--
-- /Note:/ Consider using 'billingRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vbrrsBillingRecords :: Lens.Lens' ViewBillingResponse (Core.Maybe [Types.BillingRecord])
vbrrsBillingRecords = Lens.field @"billingRecords"
{-# DEPRECATED vbrrsBillingRecords "Use generic-lens or generic-optics with 'billingRecords' instead." #-}

-- | If there are more billing records than you specified for @MaxItems@ in the request, submit another request and include the value of @NextPageMarker@ in the value of @Marker@ .
--
-- /Note:/ Consider using 'nextPageMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vbrrsNextPageMarker :: Lens.Lens' ViewBillingResponse (Core.Maybe Types.NextPageMarker)
vbrrsNextPageMarker = Lens.field @"nextPageMarker"
{-# DEPRECATED vbrrsNextPageMarker "Use generic-lens or generic-optics with 'nextPageMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vbrrsResponseStatus :: Lens.Lens' ViewBillingResponse Core.Int
vbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED vbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
