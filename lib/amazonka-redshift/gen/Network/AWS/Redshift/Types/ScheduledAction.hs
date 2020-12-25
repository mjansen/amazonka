{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ScheduledAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ScheduledAction
  ( ScheduledAction (..),

    -- * Smart constructor
    mkScheduledAction,

    -- * Lenses
    saEndTime,
    saIamRole,
    saNextInvocations,
    saSchedule,
    saScheduledActionDescription,
    saScheduledActionName,
    saStartTime,
    saState,
    saTargetAction,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.ScheduledActionState as Types
import qualified Network.AWS.Redshift.Types.ScheduledActionType as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | Describes a scheduled action. You can use a scheduled action to trigger some Amazon Redshift API operations on a schedule. For information about which API operations can be scheduled, see 'ScheduledActionType' .
--
-- /See:/ 'mkScheduledAction' smart constructor.
data ScheduledAction = ScheduledAction'
  { -- | The end time in UTC when the schedule is no longer active. After this time, the scheduled action does not trigger.
    endTime :: Core.Maybe Core.UTCTime,
    -- | The IAM role to assume to run the scheduled action. This IAM role must have permission to run the Amazon Redshift API operation in the scheduled action. This IAM role must allow the Amazon Redshift scheduler (Principal scheduler.redshift.amazonaws.com) to assume permissions on your behalf. For more information about the IAM role to use with the Amazon Redshift scheduler, see <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html Using Identity-Based Policies for Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
    iamRole :: Core.Maybe Types.String,
    -- | List of times when the scheduled action will run.
    nextInvocations :: Core.Maybe [Core.UTCTime],
    -- | The schedule for a one-time (at format) or recurring (cron format) scheduled action. Schedule invocations must be separated by at least one hour.
    --
    -- Format of at expressions is "@at(yyyy-mm-ddThh:mm:ss)@ ". For example, "@at(2016-03-04T17:27:00)@ ".
    -- Format of cron expressions is "@cron(Minutes Hours Day-of-month Month Day-of-week Year)@ ". For example, "@cron(0 10 ? * MON *)@ ". For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ .
    schedule :: Core.Maybe Types.String,
    -- | The description of the scheduled action.
    scheduledActionDescription :: Core.Maybe Types.String,
    -- | The name of the scheduled action.
    scheduledActionName :: Core.Maybe Types.String,
    -- | The start time in UTC when the schedule is active. Before this time, the scheduled action does not trigger.
    startTime :: Core.Maybe Core.UTCTime,
    -- | The state of the scheduled action. For example, @DISABLED@ .
    state :: Core.Maybe Types.ScheduledActionState,
    -- | A JSON format string of the Amazon Redshift API operation with input parameters.
    --
    -- "@{\"ResizeCluster\":{\"NodeType\":\"ds2.8xlarge\",\"ClusterIdentifier\":\"my-test-cluster\",\"NumberOfNodes\":3}}@ ".
    targetAction :: Core.Maybe Types.ScheduledActionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ScheduledAction' value with any optional fields omitted.
mkScheduledAction ::
  ScheduledAction
mkScheduledAction =
  ScheduledAction'
    { endTime = Core.Nothing,
      iamRole = Core.Nothing,
      nextInvocations = Core.Nothing,
      schedule = Core.Nothing,
      scheduledActionDescription = Core.Nothing,
      scheduledActionName = Core.Nothing,
      startTime = Core.Nothing,
      state = Core.Nothing,
      targetAction = Core.Nothing
    }

-- | The end time in UTC when the schedule is no longer active. After this time, the scheduled action does not trigger.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saEndTime :: Lens.Lens' ScheduledAction (Core.Maybe Core.UTCTime)
saEndTime = Lens.field @"endTime"
{-# DEPRECATED saEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The IAM role to assume to run the scheduled action. This IAM role must have permission to run the Amazon Redshift API operation in the scheduled action. This IAM role must allow the Amazon Redshift scheduler (Principal scheduler.redshift.amazonaws.com) to assume permissions on your behalf. For more information about the IAM role to use with the Amazon Redshift scheduler, see <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html Using Identity-Based Policies for Amazon Redshift> in the /Amazon Redshift Cluster Management Guide/ .
--
-- /Note:/ Consider using 'iamRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saIamRole :: Lens.Lens' ScheduledAction (Core.Maybe Types.String)
saIamRole = Lens.field @"iamRole"
{-# DEPRECATED saIamRole "Use generic-lens or generic-optics with 'iamRole' instead." #-}

-- | List of times when the scheduled action will run.
--
-- /Note:/ Consider using 'nextInvocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saNextInvocations :: Lens.Lens' ScheduledAction (Core.Maybe [Core.UTCTime])
saNextInvocations = Lens.field @"nextInvocations"
{-# DEPRECATED saNextInvocations "Use generic-lens or generic-optics with 'nextInvocations' instead." #-}

-- | The schedule for a one-time (at format) or recurring (cron format) scheduled action. Schedule invocations must be separated by at least one hour.
--
-- Format of at expressions is "@at(yyyy-mm-ddThh:mm:ss)@ ". For example, "@at(2016-03-04T17:27:00)@ ".
-- Format of cron expressions is "@cron(Minutes Hours Day-of-month Month Day-of-week Year)@ ". For example, "@cron(0 10 ? * MON *)@ ". For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ScheduledEvents.html#CronExpressions Cron Expressions> in the /Amazon CloudWatch Events User Guide/ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saSchedule :: Lens.Lens' ScheduledAction (Core.Maybe Types.String)
saSchedule = Lens.field @"schedule"
{-# DEPRECATED saSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The description of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saScheduledActionDescription :: Lens.Lens' ScheduledAction (Core.Maybe Types.String)
saScheduledActionDescription = Lens.field @"scheduledActionDescription"
{-# DEPRECATED saScheduledActionDescription "Use generic-lens or generic-optics with 'scheduledActionDescription' instead." #-}

-- | The name of the scheduled action.
--
-- /Note:/ Consider using 'scheduledActionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saScheduledActionName :: Lens.Lens' ScheduledAction (Core.Maybe Types.String)
saScheduledActionName = Lens.field @"scheduledActionName"
{-# DEPRECATED saScheduledActionName "Use generic-lens or generic-optics with 'scheduledActionName' instead." #-}

-- | The start time in UTC when the schedule is active. Before this time, the scheduled action does not trigger.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saStartTime :: Lens.Lens' ScheduledAction (Core.Maybe Core.UTCTime)
saStartTime = Lens.field @"startTime"
{-# DEPRECATED saStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The state of the scheduled action. For example, @DISABLED@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saState :: Lens.Lens' ScheduledAction (Core.Maybe Types.ScheduledActionState)
saState = Lens.field @"state"
{-# DEPRECATED saState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | A JSON format string of the Amazon Redshift API operation with input parameters.
--
-- "@{\"ResizeCluster\":{\"NodeType\":\"ds2.8xlarge\",\"ClusterIdentifier\":\"my-test-cluster\",\"NumberOfNodes\":3}}@ ".
--
-- /Note:/ Consider using 'targetAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saTargetAction :: Lens.Lens' ScheduledAction (Core.Maybe Types.ScheduledActionType)
saTargetAction = Lens.field @"targetAction"
{-# DEPRECATED saTargetAction "Use generic-lens or generic-optics with 'targetAction' instead." #-}

instance Core.FromXML ScheduledAction where
  parseXML x =
    ScheduledAction'
      Core.<$> (x Core..@? "EndTime")
      Core.<*> (x Core..@? "IamRole")
      Core.<*> ( x Core..@? "NextInvocations"
                   Core..<@> Core.parseXMLList "ScheduledActionTime"
               )
      Core.<*> (x Core..@? "Schedule")
      Core.<*> (x Core..@? "ScheduledActionDescription")
      Core.<*> (x Core..@? "ScheduledActionName")
      Core.<*> (x Core..@? "StartTime")
      Core.<*> (x Core..@? "State")
      Core.<*> (x Core..@? "TargetAction")
