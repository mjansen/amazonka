{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a heartbeat for the lifecycle action associated with the specified token or instance. This extends the timeout by the length of time defined using the 'PutLifecycleHook' API call.
--
-- This step is a part of the procedure for adding a lifecycle hook to an Auto Scaling group:
--
--     * (Optional) Create a Lambda function and a rule that allows CloudWatch Events to invoke your Lambda function when Amazon EC2 Auto Scaling launches or terminates instances.
--
--
--     * (Optional) Create a notification target and an IAM role. The target can be either an Amazon SQS queue or an Amazon SNS topic. The role allows Amazon EC2 Auto Scaling to publish lifecycle notifications to the target.
--
--
--     * Create the lifecycle hook. Specify whether the hook is used when the instances launch or terminate.
--
--
--     * __If you need more time, record the lifecycle action heartbeat to keep the instance in a pending state.__
--
--
--     * If you finish before the timeout period ends, complete the lifecycle action.
--
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/AutoScalingGroupLifecycle.html Auto Scaling lifecycle> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.RecordLifecycleActionHeartbeat
  ( -- * Creating a request
    RecordLifecycleActionHeartbeat (..),
    mkRecordLifecycleActionHeartbeat,

    -- ** Request lenses
    rlahLifecycleHookName,
    rlahAutoScalingGroupName,
    rlahInstanceId,
    rlahLifecycleActionToken,

    -- * Destructuring the response
    RecordLifecycleActionHeartbeatResponse (..),
    mkRecordLifecycleActionHeartbeatResponse,

    -- ** Response lenses
    rlahrrsResponseStatus,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRecordLifecycleActionHeartbeat' smart constructor.
data RecordLifecycleActionHeartbeat = RecordLifecycleActionHeartbeat'
  { -- | The name of the lifecycle hook.
    lifecycleHookName :: Types.AsciiStringMaxLen255,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | The ID of the instance.
    instanceId :: Core.Maybe Types.XmlStringMaxLen19,
    -- | A token that uniquely identifies a specific lifecycle action associated with an instance. Amazon EC2 Auto Scaling sends this token to the notification target that you specified when you created the lifecycle hook.
    lifecycleActionToken :: Core.Maybe Types.LifecycleActionToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RecordLifecycleActionHeartbeat' value with any optional fields omitted.
mkRecordLifecycleActionHeartbeat ::
  -- | 'lifecycleHookName'
  Types.AsciiStringMaxLen255 ->
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  RecordLifecycleActionHeartbeat
mkRecordLifecycleActionHeartbeat
  lifecycleHookName
  autoScalingGroupName =
    RecordLifecycleActionHeartbeat'
      { lifecycleHookName,
        autoScalingGroupName,
        instanceId = Core.Nothing,
        lifecycleActionToken = Core.Nothing
      }

-- | The name of the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleHookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlahLifecycleHookName :: Lens.Lens' RecordLifecycleActionHeartbeat Types.AsciiStringMaxLen255
rlahLifecycleHookName = Lens.field @"lifecycleHookName"
{-# DEPRECATED rlahLifecycleHookName "Use generic-lens or generic-optics with 'lifecycleHookName' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlahAutoScalingGroupName :: Lens.Lens' RecordLifecycleActionHeartbeat Types.ResourceName
rlahAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED rlahAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlahInstanceId :: Lens.Lens' RecordLifecycleActionHeartbeat (Core.Maybe Types.XmlStringMaxLen19)
rlahInstanceId = Lens.field @"instanceId"
{-# DEPRECATED rlahInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | A token that uniquely identifies a specific lifecycle action associated with an instance. Amazon EC2 Auto Scaling sends this token to the notification target that you specified when you created the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleActionToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlahLifecycleActionToken :: Lens.Lens' RecordLifecycleActionHeartbeat (Core.Maybe Types.LifecycleActionToken)
rlahLifecycleActionToken = Lens.field @"lifecycleActionToken"
{-# DEPRECATED rlahLifecycleActionToken "Use generic-lens or generic-optics with 'lifecycleActionToken' instead." #-}

instance Core.AWSRequest RecordLifecycleActionHeartbeat where
  type
    Rs RecordLifecycleActionHeartbeat =
      RecordLifecycleActionHeartbeatResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "RecordLifecycleActionHeartbeat")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "LifecycleHookName" lifecycleHookName)
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> (Core.toQueryValue "InstanceId" Core.<$> instanceId)
                Core.<> ( Core.toQueryValue "LifecycleActionToken"
                            Core.<$> lifecycleActionToken
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "RecordLifecycleActionHeartbeatResult"
      ( \s h x ->
          RecordLifecycleActionHeartbeatResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRecordLifecycleActionHeartbeatResponse' smart constructor.
newtype RecordLifecycleActionHeartbeatResponse = RecordLifecycleActionHeartbeatResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RecordLifecycleActionHeartbeatResponse' value with any optional fields omitted.
mkRecordLifecycleActionHeartbeatResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RecordLifecycleActionHeartbeatResponse
mkRecordLifecycleActionHeartbeatResponse responseStatus =
  RecordLifecycleActionHeartbeatResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rlahrrsResponseStatus :: Lens.Lens' RecordLifecycleActionHeartbeatResponse Core.Int
rlahrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rlahrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
