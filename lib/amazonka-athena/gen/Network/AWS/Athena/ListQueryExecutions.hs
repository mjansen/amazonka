{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.ListQueryExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of available query execution IDs for the queries in the specified workgroup. If a workgroup is not specified, returns a list of query execution IDs for the primary workgroup. Requires you to have access to the workgroup in which the queries ran.
--
-- For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.Athena.ListQueryExecutions
  ( -- * Creating a request
    ListQueryExecutions (..),
    mkListQueryExecutions,

    -- ** Request lenses
    lqeMaxResults,
    lqeNextToken,
    lqeWorkGroup,

    -- * Destructuring the response
    ListQueryExecutionsResponse (..),
    mkListQueryExecutionsResponse,

    -- ** Response lenses
    lqerrsNextToken,
    lqerrsQueryExecutionIds,
    lqerrsResponseStatus,
  )
where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListQueryExecutions' smart constructor.
data ListQueryExecutions = ListQueryExecutions'
  { -- | The maximum number of query executions to return in this request.
    maxResults :: Core.Maybe Core.Natural,
    -- | A token generated by the Athena service that specifies where to continue pagination if a previous request was truncated. To obtain the next set of pages, pass in the @NextToken@ from the response object of the previous page call.
    nextToken :: Core.Maybe Types.Token,
    -- | The name of the workgroup from which queries are being returned. If a workgroup is not specified, a list of available query execution IDs for the queries in the primary workgroup is returned.
    workGroup :: Core.Maybe Types.WorkGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListQueryExecutions' value with any optional fields omitted.
mkListQueryExecutions ::
  ListQueryExecutions
mkListQueryExecutions =
  ListQueryExecutions'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      workGroup = Core.Nothing
    }

-- | The maximum number of query executions to return in this request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqeMaxResults :: Lens.Lens' ListQueryExecutions (Core.Maybe Core.Natural)
lqeMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lqeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token generated by the Athena service that specifies where to continue pagination if a previous request was truncated. To obtain the next set of pages, pass in the @NextToken@ from the response object of the previous page call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqeNextToken :: Lens.Lens' ListQueryExecutions (Core.Maybe Types.Token)
lqeNextToken = Lens.field @"nextToken"
{-# DEPRECATED lqeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the workgroup from which queries are being returned. If a workgroup is not specified, a list of available query execution IDs for the queries in the primary workgroup is returned.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqeWorkGroup :: Lens.Lens' ListQueryExecutions (Core.Maybe Types.WorkGroupName)
lqeWorkGroup = Lens.field @"workGroup"
{-# DEPRECATED lqeWorkGroup "Use generic-lens or generic-optics with 'workGroup' instead." #-}

instance Core.FromJSON ListQueryExecutions where
  toJSON ListQueryExecutions {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("WorkGroup" Core..=) Core.<$> workGroup
          ]
      )

instance Core.AWSRequest ListQueryExecutions where
  type Rs ListQueryExecutions = ListQueryExecutionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonAthena.ListQueryExecutions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListQueryExecutionsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "QueryExecutionIds")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListQueryExecutions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"queryExecutionIds" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListQueryExecutionsResponse' smart constructor.
data ListQueryExecutionsResponse = ListQueryExecutionsResponse'
  { -- | A token to be used by the next request if this request is truncated.
    nextToken :: Core.Maybe Types.Token,
    -- | The unique IDs of each query execution as an array of strings.
    queryExecutionIds :: Core.Maybe (Core.NonEmpty Types.QueryExecutionId),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListQueryExecutionsResponse' value with any optional fields omitted.
mkListQueryExecutionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListQueryExecutionsResponse
mkListQueryExecutionsResponse responseStatus =
  ListQueryExecutionsResponse'
    { nextToken = Core.Nothing,
      queryExecutionIds = Core.Nothing,
      responseStatus
    }

-- | A token to be used by the next request if this request is truncated.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqerrsNextToken :: Lens.Lens' ListQueryExecutionsResponse (Core.Maybe Types.Token)
lqerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lqerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The unique IDs of each query execution as an array of strings.
--
-- /Note:/ Consider using 'queryExecutionIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqerrsQueryExecutionIds :: Lens.Lens' ListQueryExecutionsResponse (Core.Maybe (Core.NonEmpty Types.QueryExecutionId))
lqerrsQueryExecutionIds = Lens.field @"queryExecutionIds"
{-# DEPRECATED lqerrsQueryExecutionIds "Use generic-lens or generic-optics with 'queryExecutionIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lqerrsResponseStatus :: Lens.Lens' ListQueryExecutionsResponse Core.Int
lqerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lqerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
