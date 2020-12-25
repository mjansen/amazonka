{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DescribeRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified rule.
--
-- DescribeRule does not list the targets of a rule. To see the targets associated with a rule, use 'ListTargetsByRule' .
module Network.AWS.CloudWatchEvents.DescribeRule
  ( -- * Creating a request
    DescribeRule (..),
    mkDescribeRule,

    -- ** Request lenses
    drfName,
    drfEventBusName,

    -- * Destructuring the response
    DescribeRuleResponse (..),
    mkDescribeRuleResponse,

    -- ** Response lenses
    drrrsArn,
    drrrsCreatedBy,
    drrrsDescription,
    drrrsEventBusName,
    drrrsEventPattern,
    drrrsManagedBy,
    drrrsName,
    drrrsRoleArn,
    drrrsScheduleExpression,
    drrrsState,
    drrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeRule' smart constructor.
data DescribeRule = DescribeRule'
  { -- | The name of the rule.
    name :: Types.Name,
    -- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
    eventBusName :: Core.Maybe Types.EventBusNameOrArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRule' value with any optional fields omitted.
mkDescribeRule ::
  -- | 'name'
  Types.Name ->
  DescribeRule
mkDescribeRule name =
  DescribeRule' {name, eventBusName = Core.Nothing}

-- | The name of the rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfName :: Lens.Lens' DescribeRule Types.Name
drfName = Lens.field @"name"
{-# DEPRECATED drfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name or ARN of the event bus associated with the rule. If you omit this, the default event bus is used.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drfEventBusName :: Lens.Lens' DescribeRule (Core.Maybe Types.EventBusNameOrArn)
drfEventBusName = Lens.field @"eventBusName"
{-# DEPRECATED drfEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

instance Core.FromJSON DescribeRule where
  toJSON DescribeRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("EventBusName" Core..=) Core.<$> eventBusName
          ]
      )

instance Core.AWSRequest DescribeRule where
  type Rs DescribeRule = DescribeRuleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSEvents.DescribeRule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRuleResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreatedBy")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "EventBusName")
            Core.<*> (x Core..:? "EventPattern")
            Core.<*> (x Core..:? "ManagedBy")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "RoleArn")
            Core.<*> (x Core..:? "ScheduleExpression")
            Core.<*> (x Core..:? "State")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeRuleResponse' smart constructor.
data DescribeRuleResponse = DescribeRuleResponse'
  { -- | The Amazon Resource Name (ARN) of the rule.
    arn :: Core.Maybe Types.Arn,
    -- | The account ID of the user that created the rule. If you use @PutRule@ to put a rule on an event bus in another account, the other account is the owner of the rule, and the rule ARN includes the account ID for that account. However, the value for @CreatedBy@ is the account ID as the account that created the rule in the other account.
    createdBy :: Core.Maybe Types.CreatedBy,
    -- | The description of the rule.
    description :: Core.Maybe Types.Description,
    -- | The name of the event bus associated with the rule.
    eventBusName :: Core.Maybe Types.EventBusName,
    -- | The event pattern. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
    eventPattern :: Core.Maybe Types.EventPattern,
    -- | If this is a managed rule, created by an AWS service on your behalf, this field displays the principal name of the AWS service that created the rule.
    managedBy :: Core.Maybe Types.ManagedBy,
    -- | The name of the rule.
    name :: Core.Maybe Types.Name,
    -- | The Amazon Resource Name (ARN) of the IAM role associated with the rule.
    roleArn :: Core.Maybe Types.RoleArn,
    -- | The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
    scheduleExpression :: Core.Maybe Types.ScheduleExpression,
    -- | Specifies whether the rule is enabled or disabled.
    state :: Core.Maybe Types.RuleState,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeRuleResponse' value with any optional fields omitted.
mkDescribeRuleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeRuleResponse
mkDescribeRuleResponse responseStatus =
  DescribeRuleResponse'
    { arn = Core.Nothing,
      createdBy = Core.Nothing,
      description = Core.Nothing,
      eventBusName = Core.Nothing,
      eventPattern = Core.Nothing,
      managedBy = Core.Nothing,
      name = Core.Nothing,
      roleArn = Core.Nothing,
      scheduleExpression = Core.Nothing,
      state = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the rule.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsArn :: Lens.Lens' DescribeRuleResponse (Core.Maybe Types.Arn)
drrrsArn = Lens.field @"arn"
{-# DEPRECATED drrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The account ID of the user that created the rule. If you use @PutRule@ to put a rule on an event bus in another account, the other account is the owner of the rule, and the rule ARN includes the account ID for that account. However, the value for @CreatedBy@ is the account ID as the account that created the rule in the other account.
--
-- /Note:/ Consider using 'createdBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsCreatedBy :: Lens.Lens' DescribeRuleResponse (Core.Maybe Types.CreatedBy)
drrrsCreatedBy = Lens.field @"createdBy"
{-# DEPRECATED drrrsCreatedBy "Use generic-lens or generic-optics with 'createdBy' instead." #-}

-- | The description of the rule.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsDescription :: Lens.Lens' DescribeRuleResponse (Core.Maybe Types.Description)
drrrsDescription = Lens.field @"description"
{-# DEPRECATED drrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the event bus associated with the rule.
--
-- /Note:/ Consider using 'eventBusName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsEventBusName :: Lens.Lens' DescribeRuleResponse (Core.Maybe Types.EventBusName)
drrrsEventBusName = Lens.field @"eventBusName"
{-# DEPRECATED drrrsEventBusName "Use generic-lens or generic-optics with 'eventBusName' instead." #-}

-- | The event pattern. For more information, see <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-and-event-patterns.html Events and Event Patterns> in the /Amazon EventBridge User Guide/ .
--
-- /Note:/ Consider using 'eventPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsEventPattern :: Lens.Lens' DescribeRuleResponse (Core.Maybe Types.EventPattern)
drrrsEventPattern = Lens.field @"eventPattern"
{-# DEPRECATED drrrsEventPattern "Use generic-lens or generic-optics with 'eventPattern' instead." #-}

-- | If this is a managed rule, created by an AWS service on your behalf, this field displays the principal name of the AWS service that created the rule.
--
-- /Note:/ Consider using 'managedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsManagedBy :: Lens.Lens' DescribeRuleResponse (Core.Maybe Types.ManagedBy)
drrrsManagedBy = Lens.field @"managedBy"
{-# DEPRECATED drrrsManagedBy "Use generic-lens or generic-optics with 'managedBy' instead." #-}

-- | The name of the rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsName :: Lens.Lens' DescribeRuleResponse (Core.Maybe Types.Name)
drrrsName = Lens.field @"name"
{-# DEPRECATED drrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role associated with the rule.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsRoleArn :: Lens.Lens' DescribeRuleResponse (Core.Maybe Types.RoleArn)
drrrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED drrrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The scheduling expression. For example, "cron(0 20 * * ? *)", "rate(5 minutes)".
--
-- /Note:/ Consider using 'scheduleExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsScheduleExpression :: Lens.Lens' DescribeRuleResponse (Core.Maybe Types.ScheduleExpression)
drrrsScheduleExpression = Lens.field @"scheduleExpression"
{-# DEPRECATED drrrsScheduleExpression "Use generic-lens or generic-optics with 'scheduleExpression' instead." #-}

-- | Specifies whether the rule is enabled or disabled.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsState :: Lens.Lens' DescribeRuleResponse (Core.Maybe Types.RuleState)
drrrsState = Lens.field @"state"
{-# DEPRECATED drrrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrrsResponseStatus :: Lens.Lens' DescribeRuleResponse Core.Int
drrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
