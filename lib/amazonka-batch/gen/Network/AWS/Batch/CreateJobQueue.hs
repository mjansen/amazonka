{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.CreateJobQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Batch job queue. When you create a job queue, you associate one or more compute environments to the queue and assign an order of preference for the compute environments.
--
-- You also set a priority to the job queue that determines the order in which the AWS Batch scheduler places jobs onto its associated compute environments. For example, if a compute environment is associated with more than one job queue, the job queue with a higher priority is given preference for scheduling jobs to that compute environment.
module Network.AWS.Batch.CreateJobQueue
  ( -- * Creating a request
    CreateJobQueue (..),
    mkCreateJobQueue,

    -- ** Request lenses
    cjqJobQueueName,
    cjqPriority,
    cjqComputeEnvironmentOrder,
    cjqState,
    cjqTags,

    -- * Destructuring the response
    CreateJobQueueResponse (..),
    mkCreateJobQueueResponse,

    -- ** Response lenses
    cjqrrsJobQueueName,
    cjqrrsJobQueueArn,
    cjqrrsResponseStatus,
  )
where

import qualified Network.AWS.Batch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateJobQueue' smart constructor.
data CreateJobQueue = CreateJobQueue'
  { -- | The name of the job queue.
    jobQueueName :: Types.String,
    -- | The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with the same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
    priority :: Core.Int,
    -- | The set of compute environments mapped to a job queue and their order relative to each other. The job scheduler uses this parameter to determine which compute environment should execute a given job. Compute environments must be in the @VALID@ state before you can associate them with a job queue. You can associate up to three compute environments with a job queue.
    computeEnvironmentOrder :: [Types.ComputeEnvironmentOrder],
    -- | The state of the job queue. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
    state :: Core.Maybe Types.JQState,
    -- | The tags that you apply to the job queue to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJobQueue' value with any optional fields omitted.
mkCreateJobQueue ::
  -- | 'jobQueueName'
  Types.String ->
  -- | 'priority'
  Core.Int ->
  CreateJobQueue
mkCreateJobQueue jobQueueName priority =
  CreateJobQueue'
    { jobQueueName,
      priority,
      computeEnvironmentOrder = Core.mempty,
      state = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the job queue.
--
-- /Note:/ Consider using 'jobQueueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqJobQueueName :: Lens.Lens' CreateJobQueue Types.String
cjqJobQueueName = Lens.field @"jobQueueName"
{-# DEPRECATED cjqJobQueueName "Use generic-lens or generic-optics with 'jobQueueName' instead." #-}

-- | The priority of the job queue. Job queues with a higher priority (or a higher integer value for the @priority@ parameter) are evaluated first when associated with the same compute environment. Priority is determined in descending order, for example, a job queue with a priority value of @10@ is given scheduling preference over a job queue with a priority value of @1@ .
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqPriority :: Lens.Lens' CreateJobQueue Core.Int
cjqPriority = Lens.field @"priority"
{-# DEPRECATED cjqPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | The set of compute environments mapped to a job queue and their order relative to each other. The job scheduler uses this parameter to determine which compute environment should execute a given job. Compute environments must be in the @VALID@ state before you can associate them with a job queue. You can associate up to three compute environments with a job queue.
--
-- /Note:/ Consider using 'computeEnvironmentOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqComputeEnvironmentOrder :: Lens.Lens' CreateJobQueue [Types.ComputeEnvironmentOrder]
cjqComputeEnvironmentOrder = Lens.field @"computeEnvironmentOrder"
{-# DEPRECATED cjqComputeEnvironmentOrder "Use generic-lens or generic-optics with 'computeEnvironmentOrder' instead." #-}

-- | The state of the job queue. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqState :: Lens.Lens' CreateJobQueue (Core.Maybe Types.JQState)
cjqState = Lens.field @"state"
{-# DEPRECATED cjqState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The tags that you apply to the job queue to help you categorize and organize your resources. Each tag consists of a key and an optional value. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging AWS Resources> in /AWS General Reference/ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqTags :: Lens.Lens' CreateJobQueue (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
cjqTags = Lens.field @"tags"
{-# DEPRECATED cjqTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateJobQueue where
  toJSON CreateJobQueue {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("jobQueueName" Core..= jobQueueName),
            Core.Just ("priority" Core..= priority),
            Core.Just
              ("computeEnvironmentOrder" Core..= computeEnvironmentOrder),
            ("state" Core..=) Core.<$> state,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateJobQueue where
  type Rs CreateJobQueue = CreateJobQueueResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/v1/createjobqueue",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobQueueResponse'
            Core.<$> (x Core..: "jobQueueName")
            Core.<*> (x Core..: "jobQueueArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateJobQueueResponse' smart constructor.
data CreateJobQueueResponse = CreateJobQueueResponse'
  { -- | The name of the job queue.
    jobQueueName :: Types.String,
    -- | The Amazon Resource Name (ARN) of the job queue.
    jobQueueArn :: Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJobQueueResponse' value with any optional fields omitted.
mkCreateJobQueueResponse ::
  -- | 'jobQueueName'
  Types.String ->
  -- | 'jobQueueArn'
  Types.String ->
  -- | 'responseStatus'
  Core.Int ->
  CreateJobQueueResponse
mkCreateJobQueueResponse jobQueueName jobQueueArn responseStatus =
  CreateJobQueueResponse'
    { jobQueueName,
      jobQueueArn,
      responseStatus
    }

-- | The name of the job queue.
--
-- /Note:/ Consider using 'jobQueueName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqrrsJobQueueName :: Lens.Lens' CreateJobQueueResponse Types.String
cjqrrsJobQueueName = Lens.field @"jobQueueName"
{-# DEPRECATED cjqrrsJobQueueName "Use generic-lens or generic-optics with 'jobQueueName' instead." #-}

-- | The Amazon Resource Name (ARN) of the job queue.
--
-- /Note:/ Consider using 'jobQueueArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqrrsJobQueueArn :: Lens.Lens' CreateJobQueueResponse Types.String
cjqrrsJobQueueArn = Lens.field @"jobQueueArn"
{-# DEPRECATED cjqrrsJobQueueArn "Use generic-lens or generic-optics with 'jobQueueArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjqrrsResponseStatus :: Lens.Lens' CreateJobQueueResponse Core.Int
cjqrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cjqrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
