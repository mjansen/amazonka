{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.ListAttacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all ongoing DDoS attacks or all DDoS attacks during a specified time period.
--
-- This operation returns paginated results.
module Network.AWS.Shield.ListAttacks
  ( -- * Creating a request
    ListAttacks (..),
    mkListAttacks,

    -- ** Request lenses
    laEndTime,
    laMaxResults,
    laNextToken,
    laResourceArns,
    laStartTime,

    -- * Destructuring the response
    ListAttacksResponse (..),
    mkListAttacksResponse,

    -- ** Response lenses
    larrsAttackSummaries,
    larrsNextToken,
    larrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkListAttacks' smart constructor.
data ListAttacks = ListAttacks'
  { -- | The end of the time period for the attacks. This is a @timestamp@ type. The sample request above indicates a @number@ type because the default used by WAF is Unix time in seconds. However any valid <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format> is allowed.
    endTime :: Core.Maybe Types.TimeRange,
    -- | The maximum number of 'AttackSummary' objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
    --
    -- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
    maxResults :: Core.Maybe Core.Natural,
    -- | The @ListAttacksRequest.NextMarker@ value from a previous call to @ListAttacksRequest@ . Pass null if this is the first call.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The ARN (Amazon Resource Name) of the resource that was attacked. If this is left blank, all applicable resources for this account will be included.
    resourceArns :: Core.Maybe [Types.ResourceArn],
    -- | The start of the time period for the attacks. This is a @timestamp@ type. The sample request above indicates a @number@ type because the default used by WAF is Unix time in seconds. However any valid <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format> is allowed.
    startTime :: Core.Maybe Types.TimeRange
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAttacks' value with any optional fields omitted.
mkListAttacks ::
  ListAttacks
mkListAttacks =
  ListAttacks'
    { endTime = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      resourceArns = Core.Nothing,
      startTime = Core.Nothing
    }

-- | The end of the time period for the attacks. This is a @timestamp@ type. The sample request above indicates a @number@ type because the default used by WAF is Unix time in seconds. However any valid <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format> is allowed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laEndTime :: Lens.Lens' ListAttacks (Core.Maybe Types.TimeRange)
laEndTime = Lens.field @"endTime"
{-# DEPRECATED laEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The maximum number of 'AttackSummary' objects to return. If you leave this blank, Shield Advanced returns the first 20 results.
--
-- This is a maximum value. Shield Advanced might return the results in smaller batches. That is, the number of objects returned could be less than @MaxResults@ , even if there are still more objects yet to return. If there are more objects to return, Shield Advanced returns a value in @NextToken@ that you can use in your next request, to get the next batch of objects.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laMaxResults :: Lens.Lens' ListAttacks (Core.Maybe Core.Natural)
laMaxResults = Lens.field @"maxResults"
{-# DEPRECATED laMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @ListAttacksRequest.NextMarker@ value from a previous call to @ListAttacksRequest@ . Pass null if this is the first call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNextToken :: Lens.Lens' ListAttacks (Core.Maybe Types.NextToken)
laNextToken = Lens.field @"nextToken"
{-# DEPRECATED laNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The ARN (Amazon Resource Name) of the resource that was attacked. If this is left blank, all applicable resources for this account will be included.
--
-- /Note:/ Consider using 'resourceArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laResourceArns :: Lens.Lens' ListAttacks (Core.Maybe [Types.ResourceArn])
laResourceArns = Lens.field @"resourceArns"
{-# DEPRECATED laResourceArns "Use generic-lens or generic-optics with 'resourceArns' instead." #-}

-- | The start of the time period for the attacks. This is a @timestamp@ type. The sample request above indicates a @number@ type because the default used by WAF is Unix time in seconds. However any valid <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#parameter-types timestamp format> is allowed.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laStartTime :: Lens.Lens' ListAttacks (Core.Maybe Types.TimeRange)
laStartTime = Lens.field @"startTime"
{-# DEPRECATED laStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.FromJSON ListAttacks where
  toJSON ListAttacks {..} =
    Core.object
      ( Core.catMaybes
          [ ("EndTime" Core..=) Core.<$> endTime,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ResourceArns" Core..=) Core.<$> resourceArns,
            ("StartTime" Core..=) Core.<$> startTime
          ]
      )

instance Core.AWSRequest ListAttacks where
  type Rs ListAttacks = ListAttacksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSShield_20160616.ListAttacks")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttacksResponse'
            Core.<$> (x Core..:? "AttackSummaries")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAttacks where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"attackSummaries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListAttacksResponse' smart constructor.
data ListAttacksResponse = ListAttacksResponse'
  { -- | The attack information for the specified time range.
    attackSummaries :: Core.Maybe [Types.AttackSummary],
    -- | The token returned by a previous call to indicate that there is more data available. If not null, more results are available. Pass this value for the @NextMarker@ parameter in a subsequent call to @ListAttacks@ to retrieve the next set of items.
    --
    -- Shield Advanced might return the list of 'AttackSummary' objects in batches smaller than the number specified by MaxResults. If there are more attack summary objects to return, Shield Advanced will always also return a @NextToken@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAttacksResponse' value with any optional fields omitted.
mkListAttacksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAttacksResponse
mkListAttacksResponse responseStatus =
  ListAttacksResponse'
    { attackSummaries = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The attack information for the specified time range.
--
-- /Note:/ Consider using 'attackSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsAttackSummaries :: Lens.Lens' ListAttacksResponse (Core.Maybe [Types.AttackSummary])
larrsAttackSummaries = Lens.field @"attackSummaries"
{-# DEPRECATED larrsAttackSummaries "Use generic-lens or generic-optics with 'attackSummaries' instead." #-}

-- | The token returned by a previous call to indicate that there is more data available. If not null, more results are available. Pass this value for the @NextMarker@ parameter in a subsequent call to @ListAttacks@ to retrieve the next set of items.
--
-- Shield Advanced might return the list of 'AttackSummary' objects in batches smaller than the number specified by MaxResults. If there are more attack summary objects to return, Shield Advanced will always also return a @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsNextToken :: Lens.Lens' ListAttacksResponse (Core.Maybe Types.NextToken)
larrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED larrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
larrsResponseStatus :: Lens.Lens' ListAttacksResponse Core.Int
larrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED larrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
