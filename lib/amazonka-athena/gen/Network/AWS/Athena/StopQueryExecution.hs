{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.StopQueryExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a query execution. Requires you to have access to the workgroup in which the query ran.
--
-- For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
module Network.AWS.Athena.StopQueryExecution
  ( -- * Creating a request
    StopQueryExecution (..),
    mkStopQueryExecution,

    -- ** Request lenses
    sqeQueryExecutionId,

    -- * Destructuring the response
    StopQueryExecutionResponse (..),
    mkStopQueryExecutionResponse,

    -- ** Response lenses
    srsResponseStatus,
  )
where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopQueryExecution' smart constructor.
newtype StopQueryExecution = StopQueryExecution'
  { -- | The unique ID of the query execution to stop.
    queryExecutionId :: Types.QueryExecutionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopQueryExecution' value with any optional fields omitted.
mkStopQueryExecution ::
  -- | 'queryExecutionId'
  Types.QueryExecutionId ->
  StopQueryExecution
mkStopQueryExecution queryExecutionId =
  StopQueryExecution' {queryExecutionId}

-- | The unique ID of the query execution to stop.
--
-- /Note:/ Consider using 'queryExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sqeQueryExecutionId :: Lens.Lens' StopQueryExecution Types.QueryExecutionId
sqeQueryExecutionId = Lens.field @"queryExecutionId"
{-# DEPRECATED sqeQueryExecutionId "Use generic-lens or generic-optics with 'queryExecutionId' instead." #-}

instance Core.FromJSON StopQueryExecution where
  toJSON StopQueryExecution {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("QueryExecutionId" Core..= queryExecutionId)]
      )

instance Core.AWSRequest StopQueryExecution where
  type Rs StopQueryExecution = StopQueryExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonAthena.StopQueryExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopQueryExecutionResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopQueryExecutionResponse' smart constructor.
newtype StopQueryExecutionResponse = StopQueryExecutionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopQueryExecutionResponse' value with any optional fields omitted.
mkStopQueryExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopQueryExecutionResponse
mkStopQueryExecutionResponse responseStatus =
  StopQueryExecutionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopQueryExecutionResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
