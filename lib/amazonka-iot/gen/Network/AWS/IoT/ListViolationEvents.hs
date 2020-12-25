{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListViolationEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Device Defender security profile violations discovered during the given time period. You can use filters to limit the results to those alerts issued for a particular security profile, behavior, or thing (device).
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListViolationEvents
  ( -- * Creating a request
    ListViolationEvents (..),
    mkListViolationEvents,

    -- ** Request lenses
    lveStartTime,
    lveEndTime,
    lveMaxResults,
    lveNextToken,
    lveSecurityProfileName,
    lveThingName,

    -- * Destructuring the response
    ListViolationEventsResponse (..),
    mkListViolationEventsResponse,

    -- ** Response lenses
    lverrsNextToken,
    lverrsViolationEvents,
    lverrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListViolationEvents' smart constructor.
data ListViolationEvents = ListViolationEvents'
  { -- | The start time for the alerts to be listed.
    startTime :: Core.NominalDiffTime,
    -- | The end time for the alerts to be listed.
    endTime :: Core.NominalDiffTime,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A filter to limit results to those alerts generated by the specified security profile.
    securityProfileName :: Core.Maybe Types.SecurityProfileName,
    -- | A filter to limit results to those alerts caused by the specified thing.
    thingName :: Core.Maybe Types.DeviceDefenderThingName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListViolationEvents' value with any optional fields omitted.
mkListViolationEvents ::
  -- | 'startTime'
  Core.NominalDiffTime ->
  -- | 'endTime'
  Core.NominalDiffTime ->
  ListViolationEvents
mkListViolationEvents startTime endTime =
  ListViolationEvents'
    { startTime,
      endTime,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      securityProfileName = Core.Nothing,
      thingName = Core.Nothing
    }

-- | The start time for the alerts to be listed.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lveStartTime :: Lens.Lens' ListViolationEvents Core.NominalDiffTime
lveStartTime = Lens.field @"startTime"
{-# DEPRECATED lveStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end time for the alerts to be listed.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lveEndTime :: Lens.Lens' ListViolationEvents Core.NominalDiffTime
lveEndTime = Lens.field @"endTime"
{-# DEPRECATED lveEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lveMaxResults :: Lens.Lens' ListViolationEvents (Core.Maybe Core.Natural)
lveMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lveMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lveNextToken :: Lens.Lens' ListViolationEvents (Core.Maybe Types.NextToken)
lveNextToken = Lens.field @"nextToken"
{-# DEPRECATED lveNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A filter to limit results to those alerts generated by the specified security profile.
--
-- /Note:/ Consider using 'securityProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lveSecurityProfileName :: Lens.Lens' ListViolationEvents (Core.Maybe Types.SecurityProfileName)
lveSecurityProfileName = Lens.field @"securityProfileName"
{-# DEPRECATED lveSecurityProfileName "Use generic-lens or generic-optics with 'securityProfileName' instead." #-}

-- | A filter to limit results to those alerts caused by the specified thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lveThingName :: Lens.Lens' ListViolationEvents (Core.Maybe Types.DeviceDefenderThingName)
lveThingName = Lens.field @"thingName"
{-# DEPRECATED lveThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Core.AWSRequest ListViolationEvents where
  type Rs ListViolationEvents = ListViolationEventsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/violation-events",
        Core._rqQuery =
          Core.toQueryValue "startTime" startTime
            Core.<> (Core.toQueryValue "endTime" endTime)
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> ( Core.toQueryValue "securityProfileName"
                        Core.<$> securityProfileName
                    )
            Core.<> (Core.toQueryValue "thingName" Core.<$> thingName),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListViolationEventsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "violationEvents")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListViolationEvents where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"violationEvents" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListViolationEventsResponse' smart constructor.
data ListViolationEventsResponse = ListViolationEventsResponse'
  { -- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The security profile violation alerts issued for this account during the given time period, potentially filtered by security profile, behavior violated, or thing (device) violating.
    violationEvents :: Core.Maybe [Types.ViolationEvent],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListViolationEventsResponse' value with any optional fields omitted.
mkListViolationEventsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListViolationEventsResponse
mkListViolationEventsResponse responseStatus =
  ListViolationEventsResponse'
    { nextToken = Core.Nothing,
      violationEvents = Core.Nothing,
      responseStatus
    }

-- | A token that can be used to retrieve the next set of results, or @null@ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lverrsNextToken :: Lens.Lens' ListViolationEventsResponse (Core.Maybe Types.NextToken)
lverrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lverrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The security profile violation alerts issued for this account during the given time period, potentially filtered by security profile, behavior violated, or thing (device) violating.
--
-- /Note:/ Consider using 'violationEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lverrsViolationEvents :: Lens.Lens' ListViolationEventsResponse (Core.Maybe [Types.ViolationEvent])
lverrsViolationEvents = Lens.field @"violationEvents"
{-# DEPRECATED lverrsViolationEvents "Use generic-lens or generic-optics with 'violationEvents' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lverrsResponseStatus :: Lens.Lens' ListViolationEventsResponse Core.Int
lverrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lverrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
