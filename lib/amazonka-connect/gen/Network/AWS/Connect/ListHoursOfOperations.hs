{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListHoursOfOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the hours of operation for the specified Amazon Connect instance.
--
-- For more information about hours of operation, see <https://docs.aws.amazon.com/connect/latest/adminguide/set-hours-operation.html Set the Hours of Operation for a Queue> in the /Amazon Connect Administrator Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListHoursOfOperations
  ( -- * Creating a request
    ListHoursOfOperations (..),
    mkListHoursOfOperations,

    -- ** Request lenses
    lhooInstanceId,
    lhooMaxResults,
    lhooNextToken,

    -- * Destructuring the response
    ListHoursOfOperationsResponse (..),
    mkListHoursOfOperationsResponse,

    -- ** Response lenses
    lhoorrsHoursOfOperationSummaryList,
    lhoorrsNextToken,
    lhoorrsResponseStatus,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListHoursOfOperations' smart constructor.
data ListHoursOfOperations = ListHoursOfOperations'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The maximimum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHoursOfOperations' value with any optional fields omitted.
mkListHoursOfOperations ::
  -- | 'instanceId'
  Types.InstanceId ->
  ListHoursOfOperations
mkListHoursOfOperations instanceId =
  ListHoursOfOperations'
    { instanceId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhooInstanceId :: Lens.Lens' ListHoursOfOperations Types.InstanceId
lhooInstanceId = Lens.field @"instanceId"
{-# DEPRECATED lhooInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhooMaxResults :: Lens.Lens' ListHoursOfOperations (Core.Maybe Core.Natural)
lhooMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lhooMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhooNextToken :: Lens.Lens' ListHoursOfOperations (Core.Maybe Types.NextToken)
lhooNextToken = Lens.field @"nextToken"
{-# DEPRECATED lhooNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListHoursOfOperations where
  type Rs ListHoursOfOperations = ListHoursOfOperationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/hours-of-operations-summary/" Core.<> (Core.toText instanceId)),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHoursOfOperationsResponse'
            Core.<$> (x Core..:? "HoursOfOperationSummaryList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListHoursOfOperations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"hoursOfOperationSummaryList" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListHoursOfOperationsResponse' smart constructor.
data ListHoursOfOperationsResponse = ListHoursOfOperationsResponse'
  { -- | Information about the hours of operation.
    hoursOfOperationSummaryList :: Core.Maybe [Types.HoursOfOperationSummary],
    -- | If there are additional results, this is the token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListHoursOfOperationsResponse' value with any optional fields omitted.
mkListHoursOfOperationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListHoursOfOperationsResponse
mkListHoursOfOperationsResponse responseStatus =
  ListHoursOfOperationsResponse'
    { hoursOfOperationSummaryList =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the hours of operation.
--
-- /Note:/ Consider using 'hoursOfOperationSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhoorrsHoursOfOperationSummaryList :: Lens.Lens' ListHoursOfOperationsResponse (Core.Maybe [Types.HoursOfOperationSummary])
lhoorrsHoursOfOperationSummaryList = Lens.field @"hoursOfOperationSummaryList"
{-# DEPRECATED lhoorrsHoursOfOperationSummaryList "Use generic-lens or generic-optics with 'hoursOfOperationSummaryList' instead." #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhoorrsNextToken :: Lens.Lens' ListHoursOfOperationsResponse (Core.Maybe Types.NextToken)
lhoorrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lhoorrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lhoorrsResponseStatus :: Lens.Lens' ListHoursOfOperationsResponse Core.Int
lhoorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lhoorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
