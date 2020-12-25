{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.StartExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Begins the export of discovered data to an S3 bucket.
--
-- If you specify @agentIds@ in a filter, the task exports up to 72 hours of detailed data collected by the identified Application Discovery Agent, including network, process, and performance details. A time range for exported agent data may be set by using @startTime@ and @endTime@ . Export of detailed agent data is limited to five concurrently running exports.
-- If you do not include an @agentIds@ filter, summary data is exported that includes both AWS Agentless Discovery Connector data and summary data from AWS Discovery Agents. Export of summary data is limited to two exports per day.
module Network.AWS.Discovery.StartExportTask
  ( -- * Creating a request
    StartExportTask (..),
    mkStartExportTask,

    -- ** Request lenses
    setEndTime,
    setExportDataFormat,
    setFilters,
    setStartTime,

    -- * Destructuring the response
    StartExportTaskResponse (..),
    mkStartExportTaskResponse,

    -- ** Response lenses
    setrrsExportId,
    setrrsResponseStatus,
  )
where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartExportTask' smart constructor.
data StartExportTask = StartExportTask'
  { -- | The end timestamp for exported data from the single Application Discovery Agent selected in the filters. If no value is specified, exported data includes the most recent data collected by the agent.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | The file format for the returned export data. Default value is @CSV@ . __Note:__ /The/ @GRAPHML@ /option has been deprecated./
    exportDataFormat :: Core.Maybe [Types.ExportDataFormat],
    -- | If a filter is present, it selects the single @agentId@ of the Application Discovery Agent for which data is exported. The @agentId@ can be found in the results of the @DescribeAgents@ API or CLI. If no filter is present, @startTime@ and @endTime@ are ignored and exported data includes both Agentless Discovery Connector data and summary data from Application Discovery agents.
    filters :: Core.Maybe [Types.ExportFilter],
    -- | The start timestamp for exported data from the single Application Discovery Agent selected in the filters. If no value is specified, data is exported starting from the first data collected by the agent.
    startTime :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartExportTask' value with any optional fields omitted.
mkStartExportTask ::
  StartExportTask
mkStartExportTask =
  StartExportTask'
    { endTime = Core.Nothing,
      exportDataFormat = Core.Nothing,
      filters = Core.Nothing,
      startTime = Core.Nothing
    }

-- | The end timestamp for exported data from the single Application Discovery Agent selected in the filters. If no value is specified, exported data includes the most recent data collected by the agent.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setEndTime :: Lens.Lens' StartExportTask (Core.Maybe Core.NominalDiffTime)
setEndTime = Lens.field @"endTime"
{-# DEPRECATED setEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The file format for the returned export data. Default value is @CSV@ . __Note:__ /The/ @GRAPHML@ /option has been deprecated./
--
-- /Note:/ Consider using 'exportDataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setExportDataFormat :: Lens.Lens' StartExportTask (Core.Maybe [Types.ExportDataFormat])
setExportDataFormat = Lens.field @"exportDataFormat"
{-# DEPRECATED setExportDataFormat "Use generic-lens or generic-optics with 'exportDataFormat' instead." #-}

-- | If a filter is present, it selects the single @agentId@ of the Application Discovery Agent for which data is exported. The @agentId@ can be found in the results of the @DescribeAgents@ API or CLI. If no filter is present, @startTime@ and @endTime@ are ignored and exported data includes both Agentless Discovery Connector data and summary data from Application Discovery agents.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setFilters :: Lens.Lens' StartExportTask (Core.Maybe [Types.ExportFilter])
setFilters = Lens.field @"filters"
{-# DEPRECATED setFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The start timestamp for exported data from the single Application Discovery Agent selected in the filters. If no value is specified, data is exported starting from the first data collected by the agent.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setStartTime :: Lens.Lens' StartExportTask (Core.Maybe Core.NominalDiffTime)
setStartTime = Lens.field @"startTime"
{-# DEPRECATED setStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

instance Core.FromJSON StartExportTask where
  toJSON StartExportTask {..} =
    Core.object
      ( Core.catMaybes
          [ ("endTime" Core..=) Core.<$> endTime,
            ("exportDataFormat" Core..=) Core.<$> exportDataFormat,
            ("filters" Core..=) Core.<$> filters,
            ("startTime" Core..=) Core.<$> startTime
          ]
      )

instance Core.AWSRequest StartExportTask where
  type Rs StartExportTask = StartExportTaskResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSPoseidonService_V2015_11_01.StartExportTask")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartExportTaskResponse'
            Core.<$> (x Core..:? "exportId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartExportTaskResponse' smart constructor.
data StartExportTaskResponse = StartExportTaskResponse'
  { -- | A unique identifier used to query the status of an export request.
    exportId :: Core.Maybe Types.ConfigurationsExportId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartExportTaskResponse' value with any optional fields omitted.
mkStartExportTaskResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartExportTaskResponse
mkStartExportTaskResponse responseStatus =
  StartExportTaskResponse' {exportId = Core.Nothing, responseStatus}

-- | A unique identifier used to query the status of an export request.
--
-- /Note:/ Consider using 'exportId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setrrsExportId :: Lens.Lens' StartExportTaskResponse (Core.Maybe Types.ConfigurationsExportId)
setrrsExportId = Lens.field @"exportId"
{-# DEPRECATED setrrsExportId "Use generic-lens or generic-optics with 'exportId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setrrsResponseStatus :: Lens.Lens' StartExportTaskResponse Core.Int
setrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED setrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
