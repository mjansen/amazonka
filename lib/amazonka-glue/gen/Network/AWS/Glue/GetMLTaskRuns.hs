{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetMLTaskRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of runs for a machine learning transform. Machine learning task runs are asynchronous tasks that AWS Glue runs on your behalf as part of various machine learning workflows. You can get a sortable, filterable list of machine learning task runs by calling @GetMLTaskRuns@ with their parent transform's @TransformID@ and other optional parameters as documented in this section.
--
-- This operation returns a list of historic runs and must be paginated.
module Network.AWS.Glue.GetMLTaskRuns
  ( -- * Creating a request
    GetMLTaskRuns (..),
    mkGetMLTaskRuns,

    -- ** Request lenses
    gmltrTransformId,
    gmltrFilter,
    gmltrMaxResults,
    gmltrNextToken,
    gmltrSort,

    -- * Destructuring the response
    GetMLTaskRunsResponse (..),
    mkGetMLTaskRunsResponse,

    -- ** Response lenses
    gmltrrfrsNextToken,
    gmltrrfrsTaskRuns,
    gmltrrfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMLTaskRuns' smart constructor.
data GetMLTaskRuns = GetMLTaskRuns'
  { -- | The unique identifier of the machine learning transform.
    transformId :: Types.HashString,
    -- | The filter criteria, in the @TaskRunFilterCriteria@ structure, for the task run.
    filter :: Core.Maybe Types.TaskRunFilterCriteria,
    -- | The maximum number of results to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | A token for pagination of the results. The default is empty.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The sorting criteria, in the @TaskRunSortCriteria@ structure, for the task run.
    sort :: Core.Maybe Types.TaskRunSortCriteria
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetMLTaskRuns' value with any optional fields omitted.
mkGetMLTaskRuns ::
  -- | 'transformId'
  Types.HashString ->
  GetMLTaskRuns
mkGetMLTaskRuns transformId =
  GetMLTaskRuns'
    { transformId,
      filter = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sort = Core.Nothing
    }

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrTransformId :: Lens.Lens' GetMLTaskRuns Types.HashString
gmltrTransformId = Lens.field @"transformId"
{-# DEPRECATED gmltrTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The filter criteria, in the @TaskRunFilterCriteria@ structure, for the task run.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrFilter :: Lens.Lens' GetMLTaskRuns (Core.Maybe Types.TaskRunFilterCriteria)
gmltrFilter = Lens.field @"filter"
{-# DEPRECATED gmltrFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrMaxResults :: Lens.Lens' GetMLTaskRuns (Core.Maybe Core.Natural)
gmltrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gmltrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token for pagination of the results. The default is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrNextToken :: Lens.Lens' GetMLTaskRuns (Core.Maybe Types.PaginationToken)
gmltrNextToken = Lens.field @"nextToken"
{-# DEPRECATED gmltrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sorting criteria, in the @TaskRunSortCriteria@ structure, for the task run.
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrSort :: Lens.Lens' GetMLTaskRuns (Core.Maybe Types.TaskRunSortCriteria)
gmltrSort = Lens.field @"sort"
{-# DEPRECATED gmltrSort "Use generic-lens or generic-optics with 'sort' instead." #-}

instance Core.FromJSON GetMLTaskRuns where
  toJSON GetMLTaskRuns {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TransformId" Core..= transformId),
            ("Filter" Core..=) Core.<$> filter,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Sort" Core..=) Core.<$> sort
          ]
      )

instance Core.AWSRequest GetMLTaskRuns where
  type Rs GetMLTaskRuns = GetMLTaskRunsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetMLTaskRuns")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMLTaskRunsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "TaskRuns")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMLTaskRunsResponse' smart constructor.
data GetMLTaskRunsResponse = GetMLTaskRunsResponse'
  { -- | A pagination token, if more results are available.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | A list of task runs that are associated with the transform.
    taskRuns :: Core.Maybe [Types.TaskRun],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetMLTaskRunsResponse' value with any optional fields omitted.
mkGetMLTaskRunsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetMLTaskRunsResponse
mkGetMLTaskRunsResponse responseStatus =
  GetMLTaskRunsResponse'
    { nextToken = Core.Nothing,
      taskRuns = Core.Nothing,
      responseStatus
    }

-- | A pagination token, if more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrfrsNextToken :: Lens.Lens' GetMLTaskRunsResponse (Core.Maybe Types.PaginationToken)
gmltrrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gmltrrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of task runs that are associated with the transform.
--
-- /Note:/ Consider using 'taskRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrfrsTaskRuns :: Lens.Lens' GetMLTaskRunsResponse (Core.Maybe [Types.TaskRun])
gmltrrfrsTaskRuns = Lens.field @"taskRuns"
{-# DEPRECATED gmltrrfrsTaskRuns "Use generic-lens or generic-optics with 'taskRuns' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrfrsResponseStatus :: Lens.Lens' GetMLTaskRunsResponse Core.Int
gmltrrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmltrrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
