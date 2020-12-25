{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.ListSpeechSynthesisTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of SpeechSynthesisTask objects ordered by their creation date. This operation can filter the tasks by their status, for example, allowing users to list only tasks that are completed.
--
-- This operation returns paginated results.
module Network.AWS.Polly.ListSpeechSynthesisTasks
  ( -- * Creating a request
    ListSpeechSynthesisTasks (..),
    mkListSpeechSynthesisTasks,

    -- ** Request lenses
    lsstMaxResults,
    lsstNextToken,
    lsstStatus,

    -- * Destructuring the response
    ListSpeechSynthesisTasksResponse (..),
    mkListSpeechSynthesisTasksResponse,

    -- ** Response lenses
    lsstrrsNextToken,
    lsstrrsSynthesisTasks,
    lsstrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Polly.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSpeechSynthesisTasks' smart constructor.
data ListSpeechSynthesisTasks = ListSpeechSynthesisTasks'
  { -- | Maximum number of speech synthesis tasks returned in a List operation.
    maxResults :: Core.Maybe Core.Natural,
    -- | The pagination token to use in the next request to continue the listing of speech synthesis tasks.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Status of the speech synthesis tasks returned in a List operation
    status :: Core.Maybe Types.TaskStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSpeechSynthesisTasks' value with any optional fields omitted.
mkListSpeechSynthesisTasks ::
  ListSpeechSynthesisTasks
mkListSpeechSynthesisTasks =
  ListSpeechSynthesisTasks'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      status = Core.Nothing
    }

-- | Maximum number of speech synthesis tasks returned in a List operation.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsstMaxResults :: Lens.Lens' ListSpeechSynthesisTasks (Core.Maybe Core.Natural)
lsstMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lsstMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The pagination token to use in the next request to continue the listing of speech synthesis tasks.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsstNextToken :: Lens.Lens' ListSpeechSynthesisTasks (Core.Maybe Types.NextToken)
lsstNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsstNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Status of the speech synthesis tasks returned in a List operation
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsstStatus :: Lens.Lens' ListSpeechSynthesisTasks (Core.Maybe Types.TaskStatus)
lsstStatus = Lens.field @"status"
{-# DEPRECATED lsstStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.AWSRequest ListSpeechSynthesisTasks where
  type Rs ListSpeechSynthesisTasks = ListSpeechSynthesisTasksResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/v1/synthesisTasks",
        Core._rqQuery =
          Core.toQueryValue "MaxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "Status" Core.<$> status),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSpeechSynthesisTasksResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "SynthesisTasks")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListSpeechSynthesisTasks where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"synthesisTasks" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListSpeechSynthesisTasksResponse' smart constructor.
data ListSpeechSynthesisTasksResponse = ListSpeechSynthesisTasksResponse'
  { -- | An opaque pagination token returned from the previous List operation in this request. If present, this indicates where to continue the listing.
    nextToken :: Core.Maybe Types.NextToken,
    -- | List of SynthesisTask objects that provides information from the specified task in the list request, including output format, creation time, task status, and so on.
    synthesisTasks :: Core.Maybe [Types.SynthesisTask],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListSpeechSynthesisTasksResponse' value with any optional fields omitted.
mkListSpeechSynthesisTasksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListSpeechSynthesisTasksResponse
mkListSpeechSynthesisTasksResponse responseStatus =
  ListSpeechSynthesisTasksResponse'
    { nextToken = Core.Nothing,
      synthesisTasks = Core.Nothing,
      responseStatus
    }

-- | An opaque pagination token returned from the previous List operation in this request. If present, this indicates where to continue the listing.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsstrrsNextToken :: Lens.Lens' ListSpeechSynthesisTasksResponse (Core.Maybe Types.NextToken)
lsstrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsstrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | List of SynthesisTask objects that provides information from the specified task in the list request, including output format, creation time, task status, and so on.
--
-- /Note:/ Consider using 'synthesisTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsstrrsSynthesisTasks :: Lens.Lens' ListSpeechSynthesisTasksResponse (Core.Maybe [Types.SynthesisTask])
lsstrrsSynthesisTasks = Lens.field @"synthesisTasks"
{-# DEPRECATED lsstrrsSynthesisTasks "Use generic-lens or generic-optics with 'synthesisTasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsstrrsResponseStatus :: Lens.Lens' ListSpeechSynthesisTasksResponse Core.Int
lsstrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsstrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
